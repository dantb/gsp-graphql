// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// Copyright (c) 2016-2023 Grackle Contributors
//
// Licensed under the Apache License, Version 2.0 (the "License");
// you may not use this file except in compliance with the License.
// You may obtain a copy of the License at
//
//   http://www.apache.org/licenses/LICENSE-2.0
//
// Unless required by applicable law or agreed to in writing, software
// distributed under the License is distributed on an "AS IS" BASIS,
// WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
// See the License for the specific language governing permissions and
// limitations under the License.

package grackle

import scala.collection.Factory
import scala.reflect.ClassTag

import cats.MonadThrow
import cats.data.Chain
import cats.implicits._
import fs2.{ Stream, Compiler }
import io.circe.{Encoder, Json}
import io.circe.syntax._
import org.tpolecat.sourcepos.SourcePos
import org.tpolecat.typename._

import syntax._
import Cursor.{AbstractCursor, ProxyCursor}
import Query.EffectHandler
import QueryCompiler.{ComponentElaborator, EffectElaborator, IntrospectionLevel, SelectElaborator}
import QueryInterpreter.ProtoJson
import IntrospectionLevel._

/**
 * Represents a mapping between a GraphQL schema and an underlying abstract data source.
 */
abstract class Mapping[F[_]] {
  implicit val M: MonadThrow[F]
  val schema: Schema
  val typeMappings: List[TypeMapping]

  /**
    * Compile and run a single GraphQL query or mutation.
    *
    * Yields a JSON response containing the result of the query or mutation.
    */
  def compileAndRun(text: String, name: Option[String] = None, untypedVars: Option[Json] = None, introspectionLevel: IntrospectionLevel = Full, reportUnused: Boolean = true, env: Env = Env.empty)(
    implicit sc: Compiler[F,F]
  ): F[Json] =
    compileAndRunSubscription(text, name, untypedVars, introspectionLevel, reportUnused, env).compile.toList.flatMap {
      case List(j) => j.pure[F]
      case Nil     => M.raiseError(new IllegalStateException("Result stream was empty."))
      case js      => M.raiseError(new IllegalStateException(s"Result stream contained ${js.length} results; expected exactly one."))
    }

  /**
   * Compile and run a GraphQL subscription.
   *
   * Yields a stream of JSON responses containing the results of the subscription.
   */
  def compileAndRunSubscription(text: String, name: Option[String] = None, untypedVars: Option[Json] = None, introspectionLevel: IntrospectionLevel = Full, reportUnused: Boolean = true, env: Env = Env.empty): Stream[F,Json] = {
    val compiled = compiler.compile(text, name, untypedVars, introspectionLevel, reportUnused, env)
    Stream.eval(compiled.pure[F]).flatMap(_.flatTraverse(op => interpreter.run(op.query, op.rootTpe, env))).evalMap(mkResponse)
  }

  /** Combine and execute multiple queries.
   *
   *  Each query is interpreted in the context of the Cursor it is
   *  paired with. The result list is aligned with the argument
   *  query list. For each query at most one stage will be run and the
   *  corresponding result may contain deferred components.
   *
   *  Errors are aggregated across all the argument queries and are
   *  accumulated on the `Left` of the result.
   *
   *  This method is typically called at the end of a stage to evaluate
   *  deferred subqueries in the result of that stage. These will be
   *  grouped by and passed jointly to the responsible mapping in
   *  the next stage using this method. Maappongs which are able
   *  to benefit from combining queries may do so by overriding this
   *  method to implement their specific combinging logic.
   */
  def combineAndRun(queries: List[(Query, Cursor)]): F[Result[List[ProtoJson]]] =
    queries.map { case (q, c) => (q, schema.queryType, c) }.traverse((interpreter.runOneShot _).tupled).map(ProtoJson.combineResults)

  /** Yields a `Cursor` focused on the top level operation type of the query */
  def defaultRootCursor(query: Query, tpe: Type, parentCursor: Option[Cursor]): F[Result[(Query, Cursor)]] =
    Result((query, RootCursor(Context(tpe), parentCursor, Env.empty))).pure[F].widen

  /**
   * Root `Cursor` focussed on the top level operation of a query
   *
   * Construction of mapping-specific cursors is handled by delegation to
   * `mkCursorForField which is typically overridden in `Mapping` subtypes.
   */
  case class RootCursor(context: Context, parent: Option[Cursor], env: Env) extends AbstractCursor {
    def withEnv(env0: Env): Cursor = copy(env = env.add(env0))

    def focus: Any = ()

    override def hasField(fieldName: String): Boolean =
      fieldMapping(context, fieldName).isDefined

    override def field(fieldName: String, resultName: Option[String]): Result[Cursor] =
      mkCursorForField(this, fieldName, resultName)
  }

  /**
    * Yields a `Cursor` suitable for traversing the query result corresponding to
    * the `fieldName` child of `parent`.
    *
    * This method is typically overridden in and delegated to by `Mapping` subtypes.
    */
  def mkCursorForField(parent: Cursor, fieldName: String, resultName: Option[String]): Result[Cursor] = {
    val context = parent.context
    val fieldContext = context.forFieldOrAttribute(fieldName, resultName)

    def mkLeafCursor(focus: Any): Result[Cursor] =
      LeafCursor(fieldContext, focus, Some(parent), parent.env).success

    fieldMapping(context, fieldName) match {
      case Some(_ : EffectMapping) =>
        mkLeafCursor(parent.focus)
      case Some(CursorField(_, f, _, _, _)) =>
        f(parent).flatMap(res => mkLeafCursor(res))
      case _ =>
        Result.failure(s"No field '$fieldName' for type ${parent.tpe}")
    }
  }

  /** Yields the `TypeMapping` associated with the provided type, if any. */
  def typeMapping(tpe: NamedType): Option[TypeMapping] =
    typeMappingIndex.get(tpe.name)

  private lazy val typeMappingIndex =
    typeMappings.flatMap(tm => tm.tpe.asNamed.map(tpe => (tpe.name, tm)).toList).toMap

  val validator: MappingValidator =
    MappingValidator(this)

  /** Yields the `ObjectMapping` associated with the provided context, if any. */
  def objectMapping(context: Context): Option[ObjectMapping] =
    context.tpe.underlyingObject.flatMap { obj =>
      obj.asNamed.flatMap(typeMapping) match {
        case Some(om: ObjectMapping) => Some(om)
        case Some(pm: PrefixedMapping) =>
          val revPath = context.path.reverse
          pm.mappings.filter(m => revPath.endsWith(m._1)).maxByOption(_._1.length).map(_._2)
        case _ => None
      }
    }

  /** Yields the `FieldMapping` associated with `fieldName` in `context`, if any. */
  def fieldMapping(context: Context, fieldName: String): Option[FieldMapping] =
    objectMapping(context).flatMap(_.fieldMapping(fieldName)).orElse {
      context.tpe.underlyingObject match {
        case Some(ot: ObjectType) =>
          ot.interfaces.collectFirstSome(nt => fieldMapping(context.asType(nt), fieldName))
        case _ => None
      }
    }

  /** Yields the `RootEffect`, if any, associated with `fieldName`. */
  def rootEffect(context: Context, fieldName: String): Option[RootEffect] =
    fieldMapping(context, fieldName).collect {
      case re: RootEffect => re
    }

  /** Yields the `RootStream`, if any, associated with `fieldName`. */
  def rootStream(context: Context, fieldName: String): Option[RootStream] =
    fieldMapping(context, fieldName).collect {
      case rs: RootStream => rs
    }

  /** Yields the `LeafMapping` associated with the provided type, if any. */
  def leafMapping[T](tpe: Type): Option[LeafMapping[T]] =
    typeMappings.collectFirst {
      case lm@LeafMapping(tpe0, _) if tpe0 =:= tpe => lm.asInstanceOf[LeafMapping[T]]
    }

  /**
   * True if the supplied type is a leaf with respect to the GraphQL schema
   * or mapping, false otherwise.
   */
  def isLeaf(tpe: Type): Boolean = tpe.underlying match {
    case (_: ScalarType)|(_: EnumType) => true
    case tpe => leafMapping(tpe).isDefined
  }

  /** Yields the `Encoder` associated with the provided type, if any. */
  def encoderForLeaf(tpe: Type): Option[Encoder[Any]] =
    encoderMemo.get(tpe.dealias)

  private lazy val encoderMemo: scala.collection.immutable.Map[Type, Encoder[Any]] = {
    val intTypeEncoder: Encoder[Any] =
      new Encoder[Any] {
        def apply(i: Any): Json = (i: @unchecked) match {
          case i: Int => Json.fromInt(i)
          case l: Long => Json.fromLong(l)
        }
      }

    val floatTypeEncoder: Encoder[Any] =
      new Encoder[Any] {
        def apply(f: Any): Json = (f: @unchecked) match {
          case f: Float => Json.fromFloatOrString(f)
          case d: Double => Json.fromDoubleOrString(d)
          case d: BigDecimal => Json.fromBigDecimal(d)
        }
      }

    val definedEncoders: List[(Type, Encoder[Any])] =
      typeMappings.collect { case lm: LeafMapping[_] => (lm.tpe.dealias -> lm.encoder.asInstanceOf[Encoder[Any]]) }

    val defaultEncoders: List[(Type, Encoder[Any])] =
      List(
        ScalarType.StringType -> Encoder[String].asInstanceOf[Encoder[Any]],
        ScalarType.IntType -> intTypeEncoder,
        ScalarType.FloatType -> floatTypeEncoder,
        ScalarType.BooleanType -> Encoder[Boolean].asInstanceOf[Encoder[Any]],
        ScalarType.IDType -> Encoder[String].asInstanceOf[Encoder[Any]]
      )

    (definedEncoders ++ defaultEncoders).toMap
  }

  trait TypeMapping extends Product with Serializable {
    def tpe: Type
    def pos: SourcePos
  }

  case class PrimitiveMapping(tpe: Type)(implicit val pos: SourcePos) extends TypeMapping

  abstract class ObjectMapping extends TypeMapping {
    def tpe: NamedType

    private lazy val fieldMappingIndex = fieldMappings.map(fm => (fm.fieldName, fm)).toMap

    def fieldMappings: List[FieldMapping]
    def fieldMapping(fieldName: String): Option[FieldMapping] = fieldMappingIndex.get(fieldName)
  }

  object ObjectMapping {

    case class DefaultObjectMapping(tpe: NamedType, fieldMappings: List[FieldMapping])(
      implicit val pos: SourcePos
    ) extends ObjectMapping

    def apply(tpe: NamedType, fieldMappings: List[FieldMapping])(
      implicit pos: SourcePos
    ): ObjectMapping =
      DefaultObjectMapping(tpe, fieldMappings.map(_.withParent(tpe)))
  }

  case class PrefixedMapping(tpe: Type, mappings: List[(List[String], ObjectMapping)])(
    implicit val pos: SourcePos
  ) extends TypeMapping

  trait FieldMapping extends Product with Serializable {
    def fieldName: String
    def hidden: Boolean
    def withParent(tpe: Type): FieldMapping
    def pos: SourcePos
  }

  case class PrimitiveField(fieldName: String, hidden: Boolean = false)(implicit val pos: SourcePos) extends FieldMapping {
    def withParent(tpe: Type): PrimitiveField = this
  }

  /**
    * Abstract type of field mappings with effects.
    */
  trait EffectMapping extends FieldMapping

  case class EffectField(fieldName: String, handler: EffectHandler[F], required: List[String] = Nil, hidden: Boolean = false)(implicit val pos: SourcePos)
    extends EffectMapping {
    def withParent(tpe: Type): EffectField = this
  }

  /**
   * Root effects can perform an intial effect prior to computing the resulting
   * `Cursor` and effective `Query`.
   *
   * These effects are used to perform initial effectful setup for a query or to
   * perform the effect associated with a GraphQL mutation. Convenience methods
   * are provided to cover the cases where only one of the query or the cursor
   * are computed.
   *
   * If only the query is computed the default root cursor for the mapping will
   * be used. If only the cursor is computed the client query (after elaboration)
   * is used unmodified ... in this case results of the performed effect can only
   * be passed to the result construction stage via the environment associated
   * with the returned cursor.
   */
  case class RootEffect private (fieldName: String, effect: (Query, Path, Env) => F[Result[(Query, Cursor)]])(implicit val pos: SourcePos)
    extends EffectMapping {
    def hidden = false
    def withParent(tpe: Type): RootEffect = this
    def toRootStream: RootStream = RootStream(fieldName)((q, p, e) => Stream.eval(effect(q, p, e)))
  }

  object RootEffect {
    /**
     * Yields a `RootEffect` which performs both an initial effect and yields an effect-specific query and
     * corresponding root cursor.
     */
    def apply(fieldName: String)(effect: (Query, Path, Env) => F[Result[(Query, Cursor)]])(implicit pos: SourcePos, di: DummyImplicit): RootEffect =
      new RootEffect(fieldName, effect)

    /**
     * Yields a `RootEffect` which performs an initial effect which leaves the query and default root cursor
     * unchanged.
     */
    def computeUnit(fieldName: String)(effect: Env => F[Result[Unit]])(implicit pos: SourcePos): RootEffect =
      new RootEffect(
        fieldName,
        (query, path, env) =>
          (for {
            _  <- ResultT(effect(env))
            qc <- ResultT(defaultRootCursor(query, path.rootTpe, None))
          } yield qc.map(_.withEnv(env))).value
      )

    /**
      * Yields a `RootEffect` which performs an initial effect and yields an effect-specific root cursor.
      */
    def computeCursor(fieldName: String)(effect: (Path, Env) => F[Result[Cursor]])(implicit pos: SourcePos): RootEffect =
      new RootEffect(
        fieldName,
        (query, path, env) => effect(path, env).map(_.map(c => (query, c)))
      )

    /**
      * Yields a `RootEffect` which performs an initial effect and yields an effect-specific query
      * which is executed with respect to the default root cursor for the corresponding `Mapping`.
      */
    def computeChild(fieldName: String)(effect: (Query, Path, Env) => F[Result[Query]])(implicit pos: SourcePos): RootEffect =
      new RootEffect(
        fieldName,
        (query, path, env) =>
          (for {
            child <- ResultT(Query.extractChild(query).toResultOrError("Root query has unexpected shape").pure[F])
            q     <- ResultT(effect(child, path, env).map(_.flatMap(Query.substChild(query, _).toResultOrError("Root query has unexpected shape"))))
            qc    <- ResultT(defaultRootCursor(q, path.rootTpe, None))
          } yield qc.map(_.withEnv(env))).value
      )
  }

  /**
   * Root streams can perform an intial effect prior to emitting the resulting
   * cursors and effective queries.
   *
   * Stream effects are used for GraphQL subscriptions. Convenience methods are
   * provided to cover the cases where only one of the query or the cursor are
   * computed
   *
   * If only the query is computed the default root cursor for the mapping will
   * be used. If only the cursor is computed the client query (after elaboration)
   * is used unmodified ... in this case results of the performed effect can only
   * be passed to the result construction stage via the environment associated
   * with the returned cursor.
   */
  case class RootStream private (fieldName: String, effect: (Query, Path, Env) => Stream[F, Result[(Query, Cursor)]])(implicit val pos: SourcePos)
    extends EffectMapping {
    def hidden = false
    def withParent(tpe: Type): RootStream = this
  }

  object RootStream {
    /**
     * Yields a `RootStream` which performs both an initial effect and yields an effect-specific query and
     * corresponding root cursor.
     */
    def apply(fieldName: String)(effect: (Query, Path, Env) => Stream[F, Result[(Query, Cursor)]])(implicit pos: SourcePos, di: DummyImplicit): RootStream =
      new RootStream(fieldName, effect)

    /**
      * Yields a `RootStream` which yields a stream of effect-specific root cursors.
      *
      * This form of effect is typically used to implement GraphQL subscriptions.
      */
    def computeCursor(fieldName: String)(effect: (Path, Env) => Stream[F, Result[Cursor]])(implicit pos: SourcePos): RootStream =
      new RootStream(
        fieldName,
        (query, path, env) => effect(path, env).map(_.map(c => (query, c)))
      )

    /**
      * Yields a `RootStream` which yields a stream of effect-specific queries
      * which are executed with respect to the default root cursor for the
      * corresponding `Mapping`.
      *
      * This form of effect is typically used to implement GraphQL subscriptions.
      */
    def computeChild(fieldName: String)(effect: (Query, Path, Env) => Stream[F, Result[Query]])(implicit pos: SourcePos): RootStream =
      new RootStream(
        fieldName,
        (query, path, env) =>
          Query.extractChild(query).fold(Stream.emit[F, Result[(Query, Cursor)]](Result.internalError("Root query has unexpected shape"))) { child =>
            effect(child, path, env).flatMap(child0 =>
              Stream.eval(
                (for {
                  q  <- ResultT(child0.flatMap(Query.substChild(query, _).toResultOrError("Root query has unexpected shape")).pure[F])
                  qc <- ResultT(defaultRootCursor(q, path.rootTpe, None))
                } yield qc.map(_.withEnv(env))).value
              )
            )
          }
      )
  }

  trait LeafMapping[T] extends TypeMapping {
    def tpe: Type
    def encoder: Encoder[T]
    def scalaTypeName: String
    def pos: SourcePos
  }
  object LeafMapping {

    case class DefaultLeafMapping[T](tpe: Type, encoder: Encoder[T], scalaTypeName: String)(
      implicit val pos: SourcePos
    ) extends LeafMapping[T]

    def apply[T: TypeName](tpe: Type)(implicit encoder: Encoder[T], pos: SourcePos): LeafMapping[T] =
      DefaultLeafMapping(tpe, encoder, typeName)

    def unapply[T](lm: LeafMapping[T]): Option[(Type, Encoder[T])] =
      Some((lm.tpe, lm.encoder))
  }

  case class CursorField[T](fieldName: String, f: Cursor => Result[T], encoder: Encoder[T], required: List[String], hidden: Boolean)(
    implicit val pos: SourcePos
  ) extends FieldMapping {
    def withParent(tpe: Type): CursorField[T] = this
  }
  object CursorField {
    def apply[T](fieldName: String, f: Cursor => Result[T], required: List[String] = Nil, hidden: Boolean = false)(implicit encoder: Encoder[T], di: DummyImplicit): CursorField[T] =
      new CursorField(fieldName, f, encoder, required, hidden)
  }

  case class Delegate(
    fieldName: String,
    mapping: Mapping[F],
    join: (Query, Cursor) => Result[Query] = ComponentElaborator.TrivialJoin
  )(implicit val pos: SourcePos) extends FieldMapping {
    def hidden = false
    def withParent(tpe: Type): Delegate = this
  }

  val selectElaborator: SelectElaborator = SelectElaborator.identity

  lazy val componentElaborator = {
    val componentMappings =
      typeMappings.flatMap {
        case om: ObjectMapping =>
          om.fieldMappings.collect {
            case Delegate(fieldName, mapping, join) =>
              ComponentElaborator.ComponentMapping(schema.uncheckedRef(om.tpe), fieldName, mapping, join)
          }
        case _ => Nil
      }

    ComponentElaborator(componentMappings)
  }

  lazy val effectElaborator = {
    val effectMappings =
      typeMappings.flatMap {
        case om: ObjectMapping =>
          om.fieldMappings.collect {
            case EffectField(fieldName, handler, _, _) =>
              EffectElaborator.EffectMapping(schema.uncheckedRef(om.tpe), fieldName, handler)
          }
        case _ => Nil
      }

    EffectElaborator(effectMappings)
  }

  def compilerPhases: List[QueryCompiler.Phase] = List(selectElaborator, componentElaborator, effectElaborator)

  def parserConfig: GraphQLParser.Config = GraphQLParser.defaultConfig
  lazy val graphQLParser: GraphQLParser = GraphQLParser(parserConfig)
  lazy val queryParser: QueryParser = QueryParser(graphQLParser)

  lazy val compiler: QueryCompiler = new QueryCompiler(queryParser, schema, compilerPhases)

  val interpreter: QueryInterpreter[F] = new QueryInterpreter(this)

  /** Cursor positioned at a GraphQL result leaf */
  case class LeafCursor(context: Context, focus: Any, parent: Option[Cursor], env: Env) extends Cursor {
    def withEnv(env0: Env): Cursor = copy(env = env.add(env0))

    def mkChild(context: Context = context, focus: Any = focus): LeafCursor =
      LeafCursor(context, focus, Some(this), Env.empty)

    def isLeaf: Boolean = tpe.isLeaf

    def asLeaf: Result[Json] =
      encoderForLeaf(tpe).map(enc => enc(focus).success).getOrElse(Result.internalError(
        s"Cannot encode value $focus at ${context.path.reverse.mkString("/")} (of GraphQL type ${context.tpe}). Did you forget a LeafMapping?".stripMargin.trim
      ))

    def preunique: Result[Cursor] = {
      val listTpe = tpe.nonNull.list
      focus match {
        case _: List[_] => mkChild(context.asType(listTpe), focus).success
        case _ =>
          Result.internalError(s"Expected List type, found $focus for ${listTpe}")
      }
    }

    def isList: Boolean =
      tpe match {
        case ListType(_) => true
        case _ => false
      }

    def asList[C](factory: Factory[Cursor, C]): Result[C] = (tpe, focus) match {
      case (ListType(tpe), it: List[_]) => it.view.map(f => mkChild(context.asType(tpe), focus = f)).to(factory).success
      case _ => Result.internalError(s"Expected List type, found $tpe")
    }

    def listSize: Result[Int] = (tpe, focus) match {
      case (ListType(_), it: List[_]) => it.size.success
      case _ => Result.internalError(s"Expected List type, found $tpe")
    }

    def isNullable: Boolean =
      tpe match {
        case NullableType(_) => true
        case _ => false
      }

    def asNullable: Result[Option[Cursor]] =
      (tpe, focus) match {
        case (NullableType(_), None) => None.success
        case (NullableType(tpe), Some(v)) => Some(mkChild(context.asType(tpe), focus = v)).success
        case _ => Result.internalError(s"Not nullable at ${context.path}")
      }

    def isDefined: Result[Boolean] =
      (tpe, focus) match {
        case (NullableType(_), opt: Option[_]) => opt.isDefined.success
        case _ => Result.internalError(s"Not nullable at ${context.path}")
      }

    def narrowsTo(subtpe: TypeRef): Boolean = false
    def narrow(subtpe: TypeRef): Result[Cursor] =
      Result.failure(s"Cannot narrow $tpe to $subtpe")

    def hasField(fieldName: String): Boolean = false
    def field(fieldName: String, resultName: Option[String]): Result[Cursor] =
      Result.failure(s"Cannot select field '$fieldName' from leaf type $tpe")
  }

  /**
   * Proxy `Cursor` which applies a function to the focus of an underlying `LeafCursor`.
   */
  case class FieldTransformCursor[T : ClassTag : TypeName](underlying: Cursor, f: T => Result[T]) extends ProxyCursor(underlying) {
    override def withEnv(env: Env): Cursor = new FieldTransformCursor(underlying.withEnv(env), f)
    override def field(fieldName: String, resultName: Option[String]): Result[Cursor] =
      underlying.field(fieldName, resultName).flatMap {
        case l: LeafCursor =>
          for {
            focus  <- l.as[T]
            ffocus <- f(focus)
          } yield l.copy(focus = ffocus)
        case _ =>
          Result.internalError(s"Expected leaf cursor for field $fieldName")
      }
  }

  /**
   * Construct a GraphQL response from the possibly absent result `data`
   * and a collection of errors.
   */
  def mkResponse(data: Option[Json], errors: Chain[Problem]): Json = {
    val dataField = data.map { value => ("data", value) }.toList
    val fields =
      (dataField, errors.toList) match {
        case (Nil, Nil)   => List(("errors", Json.fromValues(List(Problem("Invalid query").asJson))))
        case (data, Nil)  => data
        case (data, errs) => ("errors", errs.asJson) :: data
      }
    Json.fromFields(fields)
  }

  /** Construct a GraphQL response from a `Result`. */
  def mkResponse(result: Result[Json]): F[Json] =
    result match {
      case Result.InternalError(err) => M.raiseError(err)
      case _ => mkResponse(result.toOption, result.toProblems).pure[F]
    }
}

abstract class ComposedMapping[F[_]](implicit val M: MonadThrow[F]) extends Mapping[F] {
  override def mkCursorForField(parent: Cursor, fieldName: String, resultName: Option[String]): Result[Cursor] = {
    val context = parent.context
    val fieldContext = context.forFieldOrAttribute(fieldName, resultName)
    fieldMapping(context, fieldName) match {
      case Some(_) =>
        ComposedCursor(fieldContext, parent.env).success
      case _ =>
        super.mkCursorForField(parent, fieldName, resultName)
    }
  }

  case class ComposedCursor(context: Context, env: Env) extends AbstractCursor {
    val focus = null
    val parent = None

    def withEnv(env0: Env): Cursor = copy(env = env.add(env0))

    override def hasField(fieldName: String): Boolean =
      fieldMapping(context, fieldName).isDefined

    override def field(fieldName: String, resultName: Option[String]): Result[Cursor] =
      mkCursorForField(this, fieldName, resultName)
  }
}
