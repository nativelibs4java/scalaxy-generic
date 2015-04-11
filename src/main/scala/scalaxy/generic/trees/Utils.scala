package scalaxy.generic

import scala.reflect.api.Universe

import scala.reflect.NameTransformer

private[generic] trait Utils {
  val global: Universe
  import global._

  object WithSymbol {
    def unapply(tree: Tree): Option[Symbol] = Some(tree.symbol)
  }
  object WithType {
    def unapply(tree: Tree): Option[Type] = Some(findType(tree))
  }
  object ConcreteType {
    def unapply(tpe: Type): Boolean = {
      !tpe.dealias.etaExpand.typeSymbol.asType.isAbstractType
    }
  }
  object N {
    def unapply(n: Name): Option[String] = Some(n.toString)
  }

  def findType(tree: Tree): Type =
    Option(tree.tpe).filter(_ != NoType).orElse(
      Option(tree.symbol).collect({
        case s if s.isMethod =>
          s.asMethod.returnType
        case s if s.isType =>
          s.asType.toType
      }))
      .getOrElse(NoType)

  lazy val typesToNumerics: Map[Type, Numeric[_]] = {
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

  type TreeSimplifier =
    PartialFunction[(Tree, Tree => Tree), Tree]
}
