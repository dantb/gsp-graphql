// Copyright (c) 2016-2020 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package edu.gemini.grackle
package generic

import edu.gemini.grackle._
import cats._

import scala.reflect.ClassTag

object EffQueryBuilder {
  def noArgs[F[_]] = EffQueryBuilder[F](Nil)
}

// TODO split this out into an external facing module, with internal separate. Make the intention obvious.
final case class EffQueryBuilder[F[_]](argExtractors: List[FieldArguments]) {
  def withInt(name: String)    = withArg[Int](name)
  def withId(name: String)     = withArgPf[String](name, { case Value.IDValue(s) => s })
  def withString(name: String) = withArg[String](name)
  def withFloat(name: String)  = withArg[Double](name)
  def withBool(name: String)   = withArg[Boolean](name)

  def withArg[Arg: FromQueryValue: ClassTag](name: String) =
    withArgPf[Arg](name, FromQueryValue.toPartialFunction[Arg])

  def withArgPf[Arg: ClassTag](name: String, extractor: PartialFunction[Value, Arg]): EffQueryBuilderArg1[F, Arg] =
    EffQueryBuilderArg1(Argument(name, extractor.lift))

  def build[A: CursorBuilder](query: String, process: F[A])(implicit f: MonadThrow[F]): EffQuery[F] = EffQuery.noArgs(query, process)

}

final case class EffQueryBuilderArg1[F[_], Arg1: ClassTag](arg: Argument[Arg1]) {
  def withInt(name: String)    = withArg[Int](name)
  def withId(name: String)     = withArgPf[String](name, { case Value.IDValue(s) => s })
  def withString(name: String) = withArg[String](name)
  def withFloat(name: String)  = withArg[Double](name)
  def withBool(name: String)   = withArg[Boolean](name)

  def withArg[Arg: FromQueryValue: ClassTag](name: String) =
    withArgPf[Arg](name, FromQueryValue.toPartialFunction[Arg])

  def withArgPf[Arg: ClassTag](
      name: String,
      extractor: PartialFunction[Value, Arg]
  ): EffQueryBuilderArg2[F, Arg1, Arg] =
    EffQueryBuilderArg2(arg, Argument(name, extractor.lift))

  def build[A: CursorBuilder](query: String, process: Arg1 => F[A])(implicit f: MonadThrow[F]): EffQuery[F] =
    EffQuery.arg1(query, process, arg)
}

final case class EffQueryBuilderArg2[F[_], Arg1: ClassTag, Arg2: ClassTag](
    arg1: Argument[Arg1],
    arg2: Argument[Arg2]
) {

  def build[A: CursorBuilder](query: String, process: (Arg1, Arg2) => F[A])(implicit f: MonadThrow[F]): EffQuery[F] =
    EffQuery.arg2(query, process, arg1, arg2)
}
