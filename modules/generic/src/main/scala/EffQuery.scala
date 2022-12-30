// Copyright (c) 2016-2020 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package edu.gemini.grackle
package generic

import edu.gemini.grackle._
import edu.gemini.grackle.Cursor.Env
import cats._
import cats.syntax.all._

import scala.reflect.ClassTag

final case class EffQuery[F[_]](
    topLevelField: String,
    argExtractors: List[FieldArguments],
    effectfulCursor: (Path, Env) => F[Result[Cursor]]
) {
  def buildRootEffect(mapping: GenericMapping[F]): mapping.RootEffect =
    mapping.RootEffect.computeCursor(topLevelField)((_, path, env) => effectfulCursor(path, env))
}

object EffQuery {
  def noArgs[F[_]: MonadThrow, A: CursorBuilder](topLevelField: String, process: F[A]): EffQuery[F] =
    EffQuery(topLevelField, Nil, effectfulCursor[F, A](_ => process.map(Result(_))))

  def arg1[F[_]: MonadThrow, Arg1: ClassTag, A: CursorBuilder](
      topLevelField: String,
      process: Arg1 => F[A],
      arg: Argument[Arg1]
  ): EffQuery[F] =
    EffQuery(
      topLevelField,
      List(FieldArguments(topLevelField, List(arg))),
      effectfulCursor(processWithArg[F, Arg1, A](arg.name, _, process))
    )

  def arg2[F[_]: MonadThrow, Arg1: ClassTag, Arg2: ClassTag, A: CursorBuilder](
      topLevelField: String,
      process: (Arg1, Arg2) => F[A],
      arg1: Argument[Arg1],
      arg2: Argument[Arg2]
  ): EffQuery[F] =
    EffQuery(
      topLevelField,
      List(FieldArguments(topLevelField, List(arg1, arg2))),
      effectfulCursor(processWithArg2[F, Arg1, Arg2, A](arg1.name, arg2.name, _, process.curried))
    )

  def arg3[F[_]: MonadThrow, Arg1: ClassTag, Arg2: ClassTag, Arg3: ClassTag, A: CursorBuilder](
      topLevelField: String,
      process: (Arg1, Arg2, Arg3) => F[A],
      arg1: Argument[Arg1],
      arg2: Argument[Arg2],
      arg3: Argument[Arg3]
  ): EffQuery[F] =
    EffQuery(
      topLevelField,
      List(FieldArguments(topLevelField, List(arg1, arg2, arg3))),
      effectfulCursor(processWithArg3[F, Arg1, Arg2,Arg3, A](arg1.name, arg2.name, arg3.name, _, process.curried))
    )

  def effectfulCursor[F[_]: MonadThrow, A: CursorBuilder](
      process: Env => F[Result[A]]
  )(path: Path, env: Env): F[Result[Cursor]] =
    process(env)
      .map(_.flatMap(GenericMapping.genericCursor(path, env, _)))
      .handleError(e => Result.failure[Cursor](errorMessage(e)))

  def processWithArg[F[_]: Applicative, Arg: ClassTag, A](
      argName: String,
      env: Env,
      process: Arg => F[A]
  ): F[Result[A]] =
    argOrFail[Arg](argName, env).traverse(process)

  def processWithArg2[F[_]: Applicative, Arg1: ClassTag, Arg2: ClassTag, A](
      arg1Name: String,
      arg2Name: String,
      env: Env,
      process: Arg1 => Arg2 => F[A]
  ): F[Result[A]] =
    argOrFail[Arg1](arg1Name, env).map(process).flatTraverse(processWithArg[F, Arg2, A](arg2Name, env, _))

  def processWithArg3[F[_]: Applicative, Arg1: ClassTag, Arg2: ClassTag, Arg3: ClassTag, A](
      arg1Name: String,
      arg2Name: String,
      arg3Name: String,
      env: Env,
      process: Arg1 => Arg2 => Arg3 => F[A]
  ): F[Result[A]] =
    argOrFail[Arg1](arg1Name, env)
      .map(process)
      .flatTraverse(processWithArg2[F, Arg2, Arg3, A](arg2Name, arg3Name, env, _))

  private def argOrFail[Arg1: ClassTag](name: String, env: Env) =
    env.get[Arg1](name).fold(missingArgFailure[Arg1](name, env))(Result(_))

  private def missingArgFailure[A](argName: String, env: Env): Result[A] =
    Result.failure[A](s"Missing argument `$argName` in $env")

  private def errorMessage(error: Throwable): String =
    Option(error.getMessage)
      .orElse(Option(error.getCause).map(_.getMessage))
      .getOrElse("Effectful query failed without a useful error message.")
}
