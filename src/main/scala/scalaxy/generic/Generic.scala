package scalaxy.generic

import scala.language.experimental.macros
import scala.reflect.macros.blackbox.Context

import scala.language.implicitConversions
import scala.language.dynamics

import scala.reflect.ClassTag
import scala.reflect.runtime.universe.TypeTag

// import _root_.scalaxy.generic.{ mkTypeTag => _ }

sealed class Generic[A: TypeTag] {
  def typeTag = implicitly[TypeTag[A]]
  def numeric: Option[Numeric[A]] = implicitly[Option[Numeric[A]]]
  def extremeValues: Option[ExtremeValues[A]] = implicitly[Option[ExtremeValues[A]]]
  def floatingPointValues: Option[FloatingPointValues[A]] = implicitly[Option[FloatingPointValues[A]]]
  override def toString = s"Generic[${typeTag.tpe}]"
}

object Generic {
  implicit def mkGeneric[A: TypeTag]: Generic[A] = new Generic[A]
}
