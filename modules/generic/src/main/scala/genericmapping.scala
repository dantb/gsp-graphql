// Copyright (c) 2016-2020 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package edu.gemini.grackle
package generic

import cats.Monad
import cats.implicits._
import org.tpolecat.sourcepos.SourcePos

import Cursor.{Context, DeferredCursor, Env}

abstract class GenericMapping[F[_]](implicit val M: Monad[F]) extends Mapping[F] with GenericMappingLike[F]

trait GenericMappingLike[F[_]] extends ScalaVersionSpecificGenericMappingLike[F] {
  
  type CursorBuilder[T] = edu.gemini.grackle.generic.CursorBuilder[T]

  def genericCursor[T](path: Path, env: Env, t: T)(implicit cb: => CursorBuilder[T]): Result[Cursor] = 
    GenericMapping.genericCursor(path, env, t)(cb)

  override def mkCursorForField(parent: Cursor, fieldName: String, resultName: Option[String]): Result[Cursor] = {
    val context = parent.context
    val fieldContext = context.forFieldOrAttribute(fieldName, resultName)
    fieldMapping(context, fieldName) match {
      case Some(GenericField(_, _, t, cb, _)) =>
        cb().build(fieldContext, t, Some(parent), parent.env)
      case _ =>
        super.mkCursorForField(parent, fieldName, resultName)
    }
  }

  case class GenericField[T](val tpe: Option[Type], val fieldName: String, t: T, cb: () => CursorBuilder[T], hidden: Boolean)(
    implicit val pos: SourcePos
  ) extends FieldMapping {
    def withParent(tpe: Type): GenericField[T] =
      new GenericField(Some(tpe), fieldName, t, cb, hidden)
  }

  def GenericField[T](fieldName: String, t: T, hidden: Boolean = true)(implicit cb: => CursorBuilder[T], pos: SourcePos): GenericField[T] =
    new GenericField(None, fieldName, t, () => cb, hidden)

  object semiauto {
    final def deriveObjectCursorBuilder[T](tpe: Type)
      (implicit mkBuilder: => MkObjectCursorBuilder[T]): ObjectCursorBuilder[T] = mkBuilder(tpe)
    final def deriveInterfaceCursorBuilder[T](tpe: Type)
      (implicit mkBuilder: => MkInterfaceCursorBuilder[T]): CursorBuilder[T] = mkBuilder(tpe)
  }

  trait ObjectCursorBuilder[T] extends CursorBuilder[T] {
    def renameField(from: String, to: String): ObjectCursorBuilder[T]
    def transformFieldNames(f: String => String): ObjectCursorBuilder[T]
    def transformField[U](fieldName: String)(f: T => Result[U])(implicit cb: => CursorBuilder[U]): ObjectCursorBuilder[T]
  }
}

object GenericMapping {
  def genericCursor[T](path: Path, env: Env, t: T)(implicit cb: => CursorBuilder[T]): Result[Cursor] =
    if(path.isRoot)
      cb.build(Context(path.rootTpe), t, None, env)
    else
      DeferredCursor(path, (context, parent) => cb.build(context, t, Some(parent), env)).rightIor
}