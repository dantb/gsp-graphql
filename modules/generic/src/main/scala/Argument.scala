// Copyright (c) 2016-2020 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package edu.gemini.grackle
package generic

import edu.gemini.grackle._
import edu.gemini.grackle.Query._
import cats.syntax.all._

final case class Argument[A](name: String, extract: Value => Option[A])
final case class FieldArguments(query: String, extractors: List[Argument[_]])

object FieldArguments {
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
