// Copyright (c) 2016-2020 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package edu.gemini.grackle
package generic

import edu.gemini.grackle._
import cats._

final case class EffMappingBuilder[F[_]](queries: List[EffQuery[F]]) {

  def withQuery(eq: EffQuery[F]): EffMappingBuilder[F] = EffMappingBuilder(eq :: queries)

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
          Map(rootQueryType -> FieldArguments.moveArgsToEnv(queries.flatMap(_.argExtractors)))
        )
    }
  }
}

object EffMappingBuilder {
  def empty[F[_]]: EffMappingBuilder[F] = EffMappingBuilder(Nil)
  def single[F[_]](eq: EffQuery[F])     = EffMappingBuilder(List(eq))
}
