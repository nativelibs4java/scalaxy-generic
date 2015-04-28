package scalaxy.generic

import scala.language.experimental.macros

import scala.reflect._
import scala.reflect.macros.blackbox.Context
import scala.reflect.runtime.universe

package object internal {

  def methodHomogeneous[A: c.WeakTypeTag, B: c.WeakTypeTag](c: Context)(rhs: c.Expr[A]): c.Expr[B] = {
    import c.universe._
    val Apply(Select(target, name), _) = c.macroApplication

    c.Expr[B](c.typecheck(q"${c.prefix}.applyDynamic(${name.toString})($rhs).asInstanceOf[${weakTypeOf[B]}]"))
  }

  def method0[A: c.WeakTypeTag, B: c.WeakTypeTag](c: Context): c.Expr[B] = {
    import c.universe._
    val Select(target, name) = c.macroApplication

    c.Expr[B](c.typecheck(q"${c.prefix}.selectDynamic(${name.toString}).asInstanceOf[${weakTypeOf[B]}]"))
  }
}
