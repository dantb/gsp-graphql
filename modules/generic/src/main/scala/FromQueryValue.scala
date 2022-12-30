// Copyright (c) 2016-2020 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package edu.gemini.grackle
package generic

import edu.gemini.grackle._
import cats.syntax.all._

import scala.util.Try
import java.time.ZonedDateTime

trait FromQueryValue[A] {
  def attempt(value: Value): Option[A]

  final def flatMap[B](f: A => Option[B]): FromQueryValue[B] = attempt(_).flatMap(f)
}

object FromQueryValue {
  def apply[A: FromQueryValue] = implicitly[FromQueryValue[A]]
  def toPartialFunction[A: FromQueryValue]: PartialFunction[Value, A] =
    PartialFunction.fromFunction(FromQueryValue[A].attempt).andThen { case Some(a) => a }

  implicit val string: FromQueryValue[String] = {
    case Value.StringValue(s) => Some(s)
    case _                    => None
  }

  implicit val float: FromQueryValue[Double] = {
    case Value.FloatValue(s) => Some(s)
    case _                   => None
  }

  implicit val int: FromQueryValue[Int] = {
    case Value.IntValue(s) => Some(s)
    case _                 => None
  }

  implicit val bool: FromQueryValue[Boolean] = {
    case Value.BooleanValue(s) => Some(s)
    case _                     => None
  }

  implicit val zdt: FromQueryValue[ZonedDateTime] = string.flatMap(s => Try(ZonedDateTime.parse(s)).toOption)

  implicit def opt[A: FromQueryValue]: FromQueryValue[Option[A]] = FromQueryValue[A].attempt(_).map(Option(_))

  implicit def list[A: FromQueryValue]: FromQueryValue[List[A]] = {
    case Value.ListValue(elems) => elems.traverse(x => FromQueryValue[A].attempt(x))
    case _                      => None
  }
}
