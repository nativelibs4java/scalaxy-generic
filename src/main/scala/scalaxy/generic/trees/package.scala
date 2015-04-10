package scalaxy.generic

import scala.reflect.runtime.universe._

package trees {
  private[trees] object WithSymbol {
    def unapply(tree: Tree): Option[Symbol] = Some(tree.symbol)
  }
  private[trees] object WithType {
    def unapply(tree: Tree): Option[Type] = Some(findType(tree))
  }
  private[trees] object ConcreteType {
    def unapply(tpe: Type): Boolean = {
      !tpe.dealias.etaExpand.typeSymbol.asType.isAbstractType
    }
  }
  private[trees] object N {
    def unapply(n: Name): Option[String] = Some(n.toString)
  }
}

package object trees {
  private[trees] def findType(tree: Tree): Type =
    Option(tree.tpe).filter(_ != NoType).orElse(
      Option(tree.symbol).collect({
        case s if s.isMethod =>
          s.asMethod.returnType
        case s if s.isType =>
          s.asType.toType
      }))
      .getOrElse(NoType)

  private[trees] val typesToNumerics: Map[Type, Numeric[_]] = {
    import Numeric._
    Map(
      typeOf[Byte] -> implicitly[Numeric[Byte]],
      typeOf[Short] -> implicitly[Numeric[Short]],
      typeOf[Int] -> implicitly[Numeric[Int]],
      typeOf[Long] -> implicitly[Numeric[Long]],
      typeOf[Float] -> implicitly[Numeric[Float]],
      typeOf[Double] -> implicitly[Numeric[Double]],
      typeOf[Char] -> implicitly[Numeric[Char]],
      typeOf[math.BigInt] -> implicitly[Numeric[math.BigInt]],
      typeOf[math.BigDecimal] -> implicitly[Numeric[math.BigDecimal]]
    )
  }

  def simplifyGenericTree(tree: Tree): Tree = {
    val f = GenericTrees.simplifier orElse NumericTrees.simplifier

    val transformer = new Transformer {
      val self = (tree: Tree) => transform(tree)

      override def transform(tree: Tree): Tree = {
        val sup = (tt: (Tree, Tree => Tree)) => super.transform(tt._1)
        f.applyOrElse((tree, self), sup) match {
          case tree @ q"${target @ WithType(tpe1)}.asInstanceOf[$tpe2]: $tpe3"
              if tpe1 != NoType && tpe1 =:= tpe2.tpe && tpe1 =:= tpe3.tpe =>
            target

          case tree =>
            tree
        }
      }
    }
    transformer.transform(tree)
  }
}
