# In-memory Model

The GraphQL reference implementation defines an API over a simple data model representing characters and films from
the Star Wars series. Because of its appearance in the reference implementation it is used as the basis for many
GraphQL tutorials, and many GraphQL server implementations provide it as an example. Grackle is no exception.

In this tutorial we are going to implement the Star Wars demo using Grackle backed by an in-memory model, i.e. a
simple Scala data structure which captures the information required to service the GraphQL API.

## Running the demo

The demo is packaged as submodule `demo` in the Grackle project. It is a http4s-based application which can be run
from the SBT REPL using `sbt-revolver`,

```
sbt:root> demo/reStart
[info] Application demo not yet started
[info] Starting application demo in the background ...
demo Starting demo.Main.main()
demo INFO  - Ember-Server service bound to address: [::]:8080
```

This application hosts the demo services for in-memory and db-backend models, as well as a web-based GraphQL client
(GraphQL Playground) which can be used to interact with them. You can run the client for in-memory model in your browser
at [http://localhost:8080/playground.html?endpoint=starwars](http://localhost:8080/playground.html?endpoint=starwars).

## Query examples

You can use the Playground to run queries against the model. Paste the following into the query field on left,

```yaml
query {
  hero(episode: EMPIRE) {
    name
    appearsIn
  }
}
```

Click the play button in the centre and you should see the following response on the right,

```json
{
  "data": {
    "hero": {
      "name": "Luke Skywalker",
      "appearsIn": [
        "NEWHOPE",
        "EMPIRE",
        "JEDI"
      ]
    }
  }
}
```

## The Schema

The Star Wars API is described by a GraphQL schema,

```yaml
type Query {
  hero(episode: Episode!): Character
  character(id: ID!): Character
  human(id: ID!): Human
  droid(id: ID!): Droid
}

enum Episode {
  NEWHOPE
  EMPIRE
  JEDI
}

interface Character {
  id: ID!
  name: String!
  friends: [Character]
  appearsIn: [Episode]!
}

type Droid implements Character {
  id: ID!
  name: String!
  friends: [Character]
  appearsIn: [Episode]!
  primaryFunction: String
}

type Human implements Character {
  id: ID!
  name: String!
  friends: [Character]
  appearsIn: [Episode]!
  homePlanet: String
}
```

Any one of the parametrized fields in the `Query` type may be used as the top level query, with nested queries over
fields of the result type. The structure of the query follows the schema, and the structure of the result follows the
structure of the query. For example,

```yaml
query {
  character(id: 1002) {
    name
    friends {
      name
    }
  }
}
```
yields the result,

```json
{
  "data": {
    "character": {
      "name": "Han Solo",
      "friends": [
        {
          "name": "Luke Skywalker"
        },
        {
          "name": "Leia Organa"
        },
        {
          "name": "R2-D2"
        }
      ]
    }
  }
}
```

Grackle represents schemas as Scala values of type `Schema` which can be constructed given a schema text,

```scala mdoc:passthrough
println(grackle.docs.Output.snip("demo/src/main/scala/demo/starwars/StarWarsMapping.scala", "#schema"))
```

The use of the `schema` string interpolator here causes the content of the string literal to be evaluated and checked
as a valid GraphQL schema at compile time.

## The Scala model

The API is backed by values of an ordinary Scala data types with no Grackle dependencies,

```scala mdoc:passthrough
println(grackle.docs.Output.snip("demo/src/main/scala/demo/starwars/StarWarsMapping.scala", "#model_types"))
```

The data structure is slightly complicated by the need to support cycles of friendship, e.g.,

```yaml
query {
  character(id: 1000) {
    name
    friends {
      name
      friends {
        name
      }
    }
  }
}
```

yields,

```json
{
  "data": {
    "character": {
      "name": "Luke Skywalker",
      "friends": [
        {
          "name": "Han Solo",
          "friends": [
            {
              "name": "Luke Skywalker"
            },
            ...
        }
    }
  }
}
```

Here the root of the result is "Luke Skywalker" and we loop back to Luke through the mutual friendship with Han Solo.

The data type is not itself recursive, instead friend are identified by their ids. When traversing through the list of
friends the `resolveFriends` method is used to locate the next `Character` value to visit.

```scala mdoc:passthrough
println(grackle.docs.Output.snip("demo/src/main/scala/demo/starwars/StarWarsMapping.scala", "#model_values"))
```

## The query compiler and elaborator

GraphQL queries are compiled into values of a Scala ADT which represents a query algebra. These query algebra terms
are then transformed in a variety of ways, resulting in a program which can be interpreted against the model to
produce the query result. The process of transforming these values is called _elaboration_, and each elaboration step
simplifies or expands the term to bring it into a form which can be executed directly by the query interpreter.

Grackle's query algebra consists of the following elements,

```scala
case class UntypedSelect(
  name: String, alias: Option[String],
  args: List[Binding], directives: List[Directive],
  child: Query
)
case class Select(name: String, alias: Option[String], child: Query)
case class Group(queries: List[Query])
case class Unique(child: Query)
case class Filter(pred: Predicate, child: Query)
case class Introspect(schema: Schema, child: Query)
case class Environment(env: Env, child: Query)
case class Narrow(subtpe: TypeRef, child: Query)
case class Limit(num: Int, child: Query)
case class Offset(num: Int, child: Query)
case class OrderBy(selections: OrderSelections, child: Query)
case class Count(child: Query)
case class TransformCursor(f: Cursor => Result[Cursor], child: Query)
case class Component[F[_]](mapping: Mapping[F], ...)
case class Effect[F[_]](handler: EffectHandler[F], child: Query)
case object Empty
```

A simple query like this,

```yaml
query {
  character(id: 1000) {
    name
  }
}
```

is first translated into a term in the query algebra of the form,

```scala
UntypedSelect("character", None, List(IntBinding("id", 1000)), Nil,
  UntypedSelect("name", None, Nil, Nil, Empty)
)
```

This first step is performed without reference to a GraphQL schema, hence the `id` argument is initially inferred to
be of GraphQL type `Int` rather than the type `ID` which the schema expects.

Following this initial translation the Star Wars example has a single elaboration step whose role is to translate the
selection into something executable. Elaboration uses the GraphQL schema and so is able to translate an input value
parsed as an `Int` into a GraphQL `ID`. The semantics associated with this (i.e. what an `id` is and how it relates to
the model) is specific to this model, so we have to provide that semantic via some model-specific code,

```scala mdoc:passthrough
println(grackle.docs.Output.snip("demo/src/main/scala/demo/starwars/StarWarsMapping.scala", "#elaborator"))
```

Extracting out the case for the `character` selector,

```scala
case (QueryType, "character", List(Binding("id", IDValue(id)))) =>
  Elab.transformChild { child =>
    Unique(Filter(Eql(CharacterType / "id", Const(id)), child))
  }
```

the previous term is transformed as follows,

```scala
Select("character", None,
  Unique(Eql(CharacterType / "id"), Const("1000")), Select("name", None, Empty))
)
```

Here the original `UntypedSelect` terms have been converted to typed `Select` terms with the argument to the
`character` selector translated into a predicate which refines the root data of the model to the single element which
satisfies it via `Unique`. The remainder of the query (`Select("name", None, Nil)`) is then within the scope of that
constraint. We have eliminated something with model-specific semantics (`character(id: 1000)`) in favour of something
universal which can be interpreted directly against the model.

## The query interpreter and cursor

The data required to construct the response to a query is determined by the structure of the query and gives rise to a
more or less arbitrary traversal of the model. To support this Grackle provides a functional `Cursor` abstraction
which points into the model and can navigate through GraphQL fields, arrays and values as required by a given query.

For in-memory models where the structure of the model ADT closely matches the GraphQL schema a `Cursor` can be derived
automatically by a `GenericMapping` which only needs to be supplemented with a specification of the root mappings for
the top level fields of the GraphQL schema. The root mappings enable the query interpreter to construct an appropriate
initial `Cursor` for the query being evaluated.

For the Star Wars model the root definitions are of the following form,

```scala mdoc:passthrough
println(grackle.docs.Output.snip("demo/src/main/scala/demo/starwars/StarWarsMapping.scala", "#root"))
```

The first argument of the `GenericField` constructor corresponds to the top-level selection of the query (see the
schema above) and the second argument is the initial model value for which a `Cursor` will be derived.  When the query
is executed, navigation will start with that `Cursor` and the corresponding GraphQL type.

## The service

What we've seen so far allows us to compile and execute GraphQL queries against our in-memory model. We now need to
expose that via HTTP. The following shows how we do that for http4s,

```scala mdoc:passthrough
println(grackle.docs.Output.snip("demo/src/main/scala/demo/GraphQLService.scala", "#service"))
```

The GraphQL specification supports queries both via HTTP GET and POST requests, so we provide routes for both methods.
For queries via GET the query is embedded in the URI query string in the form `... ?query=<URI encoded GraphQL
query>`. For queries via POST, the query is embedded in a JSON value of the form,

```json
{
  "operationName": "Query",
  "query": "character(id: 1000) ..."
}
```

In each of these cases we extract the operation name and query from the request and pass them to the service for
compilation and execution.

Many GraphQL client tools expect servers to be able to respond to a query named `IntrospectionQuery` returning a
representation of the GraphQL schema supported by the endpoint which they use to provide client-side highlighting,
validation, auto completion etc. The demo service provides this as well as normal query execution.

## Putting it all together

Finally we need to run all of this on top of http4s. Here we have a simple `IOApp` running an `EmberServer` with the
`StarWarsService` defined above, and a `ResourceService` to serve the GraphQL Playground web client,

```scala
object Main extends IOApp {
  def run(args: List[String]): IO[ExitCode] = {
    (for {
      starWarsRoutes <- StarWarsMapping[IO].map(mkRoutes("starwars"))
      _              <- mkServer(starWarsRoutes)
    } yield ()).useForever
  }
}
```

```scala mdoc:passthrough
println(grackle.docs.Output.snip("demo/src/main/scala/demo/DemoServer.scala", "#server"))
```
