// Copyright (c) 2016-2020 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package edu.gemini.grackle
package generic

import edu.gemini.grackle._
import edu.gemini.grackle.Query._
import edu.gemini.grackle.Cursor.Env
import cats._
import cats.syntax.all._

import scala.reflect.ClassTag

final case class EffectfulMappingBuilder[F[_]](queries: List[EffectfulQuery[F]]) {

  def withQuery(eq: EffectfulQuery[F]): EffectfulMappingBuilder[F] = EffectfulMappingBuilder(eq :: queries)

  // NOTE - there's an assumption that schema must have a Query top-level type. This would go in docs.
  def build(schema0: Schema)(implicit m: Monad[F]): GenericMapping[F] = {
    val rootQueryType = schema0.ref("Query")
    new GenericMapping[F] {
      def effects: List[RootEffect]     = queries.map(_.buildRootEffect(this))
      final override val schema: Schema = schema0
      final override val typeMappings: List[TypeMapping] =
        List(ObjectMapping(rootQueryType, effects))
      final override val selectElaborator: QueryCompiler.SelectElaborator =
        new QueryCompiler.SelectElaborator(
          Map(rootQueryType -> SelectArguments.moveArgsToEnv(queries.flatMap(_.argExtractors)))
        )
    }
  }
}

object EffectfulMappingBuilder {
  def empty[F[_]]: EffectfulMappingBuilder[F] = EffectfulMappingBuilder(Nil)
  def single[F[_]](eq: EffectfulQuery[F]) = EffectfulMappingBuilder(List(eq))
}

sealed trait EffectfulQuery[F[_]] {
  def topLevelQuery: String
  def argExtractors: List[SelectArguments]
  def effectfulCursor(path: Path, env: Env): F[Result[Cursor]]

  final def buildRootEffect(mapping: GenericMapping[F]): mapping.RootEffect =
    mapping.RootEffect.computeCursor(topLevelQuery)((_, path, env) => effectfulCursor(path, env))
}

object EffectfulQuery {
  private def apply[F[_]](query: String, extractors: List[SelectArguments], cursor: (Path, Env) => F[Result[Cursor]]) =
    new EffectfulQuery[F] {
      val topLevelQuery: String                                    = query
      def argExtractors: List[SelectArguments]                     = extractors
      def effectfulCursor(path: Path, env: Env): F[Result[Cursor]] = cursor(path, env)
    }

  def noArgs[F[_]: MonadThrow, A: CursorBuilder](topLevelQuery: String, process: F[A]): EffectfulQuery[F] =
    EffectfulQuery(
      topLevelQuery,
      Nil,
      (path, env) => effectfulCursor[F, A](_ => process.map(Result(_)))(path, env)
    )

  def arg1[F[_]: MonadThrow, Arg1: ClassTag, A: CursorBuilder](
      topLevelQuery: String,
      process: Arg1 => F[A],
      argName: String,
      extractor: PartialFunction[Value, Arg1]
  ): EffectfulQuery[F] =
    EffectfulQuery(
      topLevelQuery,
      List(SelectArguments(topLevelQuery, List(ArgumentExtractor(argName, extractor.lift)))),
      (path, env) =>
        effectfulCursor {
          processWithArg[F, Arg1, A](argName, _)(process)
        }(path, env)
    )

  def arg2[F[_]: MonadThrow, Arg1: ClassTag, Arg2: ClassTag, A: CursorBuilder](
      topLevelQuery: String,
      process: (Arg1, Arg2) => F[A],
      argName1: String,
      argName2: String,
      extractor1: PartialFunction[Value, Arg1],
      extractor2: PartialFunction[Value, Arg2]
  ): EffectfulQuery[F] =
    EffectfulQuery(
      topLevelQuery,
      List(
        SelectArguments(
          topLevelQuery,
          List(ArgumentExtractor(argName1, extractor1.lift), ArgumentExtractor(argName2, extractor2.lift))
        )
      ),
      (path, env) =>
        effectfulCursor {
          processWithArg2[F, Arg1, Arg2, A](argName1, argName2, process)
        }(path, env)
    )

  private def effectfulCursor[F[_]: MonadThrow, A: CursorBuilder](
      process: Env => F[Result[A]]
  )(path: Path, env: Env): F[Result[Cursor]] =
    process(env)
      .map(_.flatMap(GenericMapping.genericCursor(path, env, _)))
      .handleError(e => Result.failure[Cursor](errorMessage(e)))

  private def processWithArg[F[_]: Applicative, Arg: ClassTag, A](argName: String, env: Env)(
      process: Arg => F[A]
  ): F[Result[A]] =
    env.get[Arg](argName).fold(missingArgFailure[A](argName, env).pure[F])(process(_).map(Result(_)))

  private def processWithArg2[F[_]: Applicative, Arg1: ClassTag, Arg2: ClassTag, A](
      arg1Name: String,
      arg2Name: String,
      process: (Arg1, Arg2) => F[A]
  )(
      env: Env
  ): F[Result[A]] =
    (for {
      arg1 <-env.get[Arg1](arg1Name).fold(missingArgFailure[Arg1](arg1Name, env))(x => Result(x))
      arg2 <- env.get[Arg2](arg2Name).fold(missingArgFailure[Arg2](arg2Name, env))(x => Result(x))
    } yield process(arg1, arg2)).sequence

  private def missingArgFailure[A](argName: String, env: Env): Result[A] = Result.failure[A](s"Missing argument `$argName` in $env")

  private def errorMessage(error: Throwable): String =
    Option(error.getMessage)
      .orElse(Option(error.getCause).map(_.getMessage))
      .getOrElse("Effectful query failed without a useful error message.")
}

final case class SelectArguments(query: String, extractors: List[ArgumentExtractor])
final case class ArgumentExtractor(name: String, extract: Value => Option[Any])

object SelectArguments {
  def moveArgsToEnv(selectArguments: List[SelectArguments]): PartialFunction[Select, Result[Query]] =
    PartialFunction
      .fromFunction[Select, Option[Result[Query]]] {
        case Select(query, bindings, child) =>
          selectArguments.collectFirst {
            case se if se.query === query => Result(Environment(buildEnv(bindings, se), Select(query, Nil, child)))
          }
      }
      .andThen { case Some(value) => value }

  def buildEnv(bindings: List[Binding], se: SelectArguments): Cursor.Env =
    bindings.foldLeft(Cursor.Env.empty) {
      case (accEnv, Binding(name, value)) =>
        val newEnv = se.extractors
          .collectFirst { case e if e.name === name => e.extract(value) }
          .map(e => Cursor.Env(name -> e))
          .getOrElse(Cursor.Env.empty)
        accEnv.add(newEnv)
    }
}
