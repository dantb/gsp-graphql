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

sealed trait EffectfulQuery[F[_]] {
  def topLevelQuery: String
  def argExtractors: List[FieldArguments]
  def effectfulCursor(path: Path, env: Env): F[Result[Cursor]]

  final def buildRootEffect(mapping: GenericMapping[F]): mapping.RootEffect =
    mapping.RootEffect.computeCursor(topLevelQuery)((_, path, env) => effectfulCursor(path, env))
}

object EffectfulQuery {
  def apply[F[_]](query: String, extractors: List[FieldArguments], cursor: (Path, Env) => F[Result[Cursor]]) =
    new EffectfulQuery[F] {
      val topLevelQuery: String                                    = query
      def argExtractors: List[FieldArguments]                      = extractors
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
      List(FieldArguments(topLevelQuery, List(ArgumentExtractor(argName, extractor.lift, implicitly[ClassTag[Arg1]])))),
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
        FieldArguments(
          topLevelQuery,
          List(
            ArgumentExtractor(argName1, extractor1.lift, implicitly[ClassTag[Arg1]]),
            ArgumentExtractor(argName2, extractor2.lift, implicitly[ClassTag[Arg2]])
          )
        )
      ),
      (path, env) =>
        effectfulCursor {
          processWithArg2[F, Arg1, Arg2, A](argName1, argName2, process)
        }(path, env)
    )

  def effectfulCursor[F[_]: MonadThrow, A: CursorBuilder](
      process: Env => F[Result[A]]
  )(path: Path, env: Env): F[Result[Cursor]] =
    process(env)
      .map(_.flatMap(GenericMapping.genericCursor(path, env, _)))
      .handleError(e => Result.failure[Cursor](errorMessage(e)))

  def processWithArg[F[_]: Applicative, Arg: ClassTag, A](argName: String, env: Env)(
      process: Arg => F[A]
  ): F[Result[A]] =
    env.get[Arg](argName).fold(missingArgFailure[A](argName, env).pure[F])(process(_).map(Result(_)))

  def processWithArg2[F[_]: Applicative, Arg1: ClassTag, Arg2: ClassTag, A](
      arg1Name: String,
      arg2Name: String,
      process: (Arg1, Arg2) => F[A]
  )(
      env: Env
  ): F[Result[A]] =
    (for {
      arg1 <- env.get[Arg1](arg1Name).fold(missingArgFailure[Arg1](arg1Name, env))(x => Result(x))
      arg2 <- env.get[Arg2](arg2Name).fold(missingArgFailure[Arg2](arg2Name, env))(x => Result(x))
    } yield process(arg1, arg2)).sequence

  private def missingArgFailure[A](argName: String, env: Env): Result[A] =
    Result.failure[A](s"Missing argument `$argName` in $env")

  private def errorMessage(error: Throwable): String =
    Option(error.getMessage)
      .orElse(Option(error.getCause).map(_.getMessage))
      .getOrElse("Effectful query failed without a useful error message.")
}

final case class FieldArguments(query: String, extractors: List[ArgumentExtractor[_]])
final case class ArgumentExtractor[A](name: String, extract: Value => Option[A], ct: ClassTag[A])

object FieldArguments {
  def apply[Arg: ClassTag](query: String, argName: String, extractor: PartialFunction[Value, Arg]): FieldArguments =
    FieldArguments(query, List(ArgumentExtractor(argName, extractor.lift, implicitly[ClassTag[Arg]])))

  def moveArgsToEnv(fieldArguments: List[FieldArguments]): PartialFunction[Select, Result[Query]] =
    PartialFunction
      .fromFunction[Select, Option[Result[Query]]] {
        case Select(query, bindings, child) =>
          fieldArguments.collectFirst {
            case se if se.query === query => Result(Environment(buildEnv(bindings, se), Select(query, Nil, child)))
          }
      }
      .andThen { case Some(value) => value }

  def buildEnv(bindings: List[Binding], se: FieldArguments): Cursor.Env =
    bindings.foldLeft(Cursor.Env.empty) {
      case (accEnv, Binding(name, value)) =>
        val newEnv = se.extractors
          .collectFirst { case e if e.name === name => e.extract(value) }
          .map(e => Cursor.Env(name -> e))
          .getOrElse(Cursor.Env.empty)
        accEnv.add(newEnv)
    }
}
