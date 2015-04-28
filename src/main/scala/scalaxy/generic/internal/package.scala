package scalaxy.generic

import scala.language.experimental.macros

import scala.reflect._
import scala.reflect.macros.blackbox.Context
import scala.reflect.runtime.universe

package object internal {

  def methodHomogeneous[A: c.WeakTypeTag, B: c.WeakTypeTag](c: Context)(rhs: c.Expr[A]): c.Expr[B] = {
    import c.universe._
    val Apply(Select(target, name), _) = c.macroApplication

    c.Expr[B](q"${c.prefix}.applyDynamic(${name.toString})($rhs).asInstanceOf[${weakTypeOf[B]}]")
  }

  def method0[A: c.WeakTypeTag, B: c.WeakTypeTag](c: Context): c.Expr[B] = {
    import c.universe._
    val Select(target, name) = c.macroApplication

    c.Expr[B](q"${c.prefix}.selectDynamic(${name.toString}).asInstanceOf[${weakTypeOf[B]}]")
  }

  def maxValue[A : c.WeakTypeTag](c: Context)(ev: c.Expr[Generic[A]]): c.Expr[A] =
    getFirstMatchingMember[A](c)("MaxValue", "MAX_VALUE")

  def minPositiveValue[A : c.WeakTypeTag](c: Context)(ev: c.Expr[Generic[A]]): c.Expr[A] =
    getFirstMatchingMember[A](c)("MinPositiveValue")

  def minValue[A : c.WeakTypeTag](c: Context)(ev: c.Expr[Generic[A]]): c.Expr[A] =
    getFirstMatchingMember[A](c)("MinValue", "MIN_VALUE")

  def positiveInfinity[A : c.WeakTypeTag](c: Context)(ev: c.Expr[Generic[A]]): c.Expr[A] =
    getFirstMatchingMember[A](c)("PositiveInfinity")

  def negativeInfinity[A : c.WeakTypeTag](c: Context)(ev: c.Expr[Generic[A]]): c.Expr[A] =
    getFirstMatchingMember[A](c)("NegativeInfinity")

  def NaN[A : c.WeakTypeTag](c: Context)(ev: c.Expr[Generic[A]]): c.Expr[A] =
    getFirstMatchingMember[A](c)("NaN")

  private[this]
  def getFirstMatchingMember[A : c.WeakTypeTag](c: Context)(candidateNames: String*): c.Expr[A] = {
    import c.universe._

    val tpe = weakTypeOf[A]
    val moduleSym = tpe.typeSymbol.companion
    val moduleTpe = tpe.companion

    val sym = candidateNames.map(name => moduleTpe member TermName(name)).reduce(_ orElse _)

    if (sym != NoSymbol) {
      c.Expr[A](c.typecheck(q"$moduleSym.${sym.asTerm.name}"))
    } else {
      c.error(c.enclosingPosition,
        s"Could not find a member matching any of ${candidateNames.mkString("[", ", ", "]")} in $moduleSym")
      c.Expr[A](q"null")
    }
  }
}
