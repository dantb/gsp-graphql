// Copyright (c) 2016-2020 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package edu.gemini.grackle
package generic

import edu.gemini.grackle._
import cats._
import cats.syntax.all._

import scala.reflect.ClassTag

final case class Argument[A](name: String, extractor: PartialFunction[Value, A])

object EffectfulQueryBuilder {
  def noArgs[F[_]](query: String) = EffectfulQueryBuilder0[F](query, Nil)
}

// TODO split this out into an external facing module, with internal separate. Make the intention obvious.
final case class EffectfulQueryBuilder0[F[_]](query: String, argExtractors: List[FieldArguments]) {
  def withInt(name: String)    = withArg[Int](name, { case Value.IntValue(s) => s })
  def withId(name: String)     = withArg[String](name, { case Value.IDValue(s) => s })
  def withString(name: String) = withArg[String](name, { case Value.StringValue(s) => s })
  def withFloat(name: String)  = withArg[Double](name, { case Value.FloatValue(s) => s })
  def withBool(name: String)   = withArg[Boolean](name, { case Value.BooleanValue(s) => s })
  def withArg[Arg: ClassTag](name: String, extractor: PartialFunction[Value, Arg]): EffectfulQueryBuilderArg1[F, Arg] =
    EffectfulQueryBuilderArg1(query, Argument(name, extractor))

  def build[A: CursorBuilder](process: F[A])(implicit f: MonadThrow[F]): EffectfulQuery[F] =
    EffectfulQuery(
      query,
      argExtractors,
      (path, env) => EffectfulQuery.effectfulCursor[F, A](_ => process.map(Result(_)))(path, env)
    )

}

final case class EffectfulQueryBuilderArg1[F[_], Arg1: ClassTag](query: String, arg: Argument[Arg1]) {

  def withInt(name: String)    = withArg[Int](name, { case Value.IntValue(s) => s })
  def withId(name: String)     = withArg[String](name, { case Value.IDValue(s) => s })
  def withString(name: String) = withArg[String](name, { case Value.StringValue(s) => s })
  def withFloat(name: String)  = withArg[Double](name, { case Value.FloatValue(s) => s })
  def withBool(name: String)   = withArg[Boolean](name, { case Value.BooleanValue(s) => s })
  def withArg[Arg: ClassTag](name: String, extractor: PartialFunction[Value, Arg]) =
    EffectfulQueryBuilderArg2(query, arg, Argument(name, extractor))

  def build[A: CursorBuilder](process: Arg1 => F[A])(implicit f: MonadThrow[F]): EffectfulQuery[F] =
    EffectfulQuery(
      query,
      List(FieldArguments(query, arg.name, arg.extractor)),
      (path, env) =>
        EffectfulQuery.effectfulCursor {
          EffectfulQuery.processWithArg[F, Arg1, A](arg.name, _)(process)
        }(path, env)
    )
}

final case class EffectfulQueryBuilderArg2[F[_], Arg1: ClassTag, Arg2: ClassTag](
    query: String,
    arg1: Argument[Arg1],
    arg2: Argument[Arg2]
) {

  def build[A: CursorBuilder](process: (Arg1, Arg2) => F[A])(implicit f: MonadThrow[F]): EffectfulQuery[F] =
    EffectfulQuery(
      query,
      List(FieldArguments(query, arg1.name, arg1.extractor), FieldArguments(query, arg2.name, arg2.extractor)),
      (path, env) =>
        EffectfulQuery.effectfulCursor {
          EffectfulQuery.processWithArg2[F, Arg1, Arg2, A](arg1.name, arg2.name, process)
        }(path, env)
    )
}
