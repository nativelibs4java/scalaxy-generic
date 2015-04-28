package scalaxy

import scala.language.higherKinds
import scala.language.implicitConversions
import scala.language.dynamics
import scala.language.experimental.macros

import scala.reflect.runtime.universe._

package object generic extends CommonValues {

  def generic[A: Generic](value: A) = new GenericOps[A](value)

  private[this] def typeMap[B](pairs: (TypeTag[_], B)*): Map[Type, B] =
    Map(pairs.map { case (tt, value) => tt.tpe -> value }: _*)

  private[generic] val typesToNumerics: Map[Type, Numeric[_]] = {
    import Numeric._
    typeMap(
      TypeTag.Byte -> implicitly[Numeric[Byte]],
      TypeTag.Short -> implicitly[Numeric[Short]],
      TypeTag.Int -> implicitly[Numeric[Int]],
      TypeTag.Long -> implicitly[Numeric[Long]],
      TypeTag.Float -> implicitly[Numeric[Float]],
      TypeTag.Double -> implicitly[Numeric[Double]],
      TypeTag.Char -> implicitly[Numeric[Char]],
      typeTag[math.BigInt] -> implicitly[Numeric[math.BigInt]],
      typeTag[math.BigDecimal] -> implicitly[Numeric[math.BigDecimal]]
    )
  }

  private[generic] val typesToExtremeValues: Map[Type, ExtremeValues[_]] = {
    typeMap(
      TypeTag.Byte -> ByteValues,
      TypeTag.Short -> ShortValues,
      TypeTag.Int -> IntValues,
      TypeTag.Long -> LongValues,
      TypeTag.Float -> FloatValues,
      TypeTag.Double -> DoubleValues
    )
  }

  private[generic] val typesToFloatingPointValues: Map[Type, FloatingPointValues[_]] = {
    typeMap(
      TypeTag.Float -> FloatValues,
      TypeTag.Double -> DoubleValues
    )
  }

  def genericTypeTag[A: Generic]: TypeTag[A] = implicitly[Generic[A]].typeTag

  private[this]
  def lookupByType[A : TypeTag, B[A]](map: Map[Type, B[_]]): Option[B[A]] =
    map.get(implicitly[TypeTag[A]].tpe.dealias.etaExpand).map(_.asInstanceOf[B[A]])

  implicit def mkNumeric[A: TypeTag]: Option[Numeric[A]] =
    lookupByType[A, Numeric](typesToNumerics)

  implicit def mkExtremeValues[A: TypeTag]: Option[ExtremeValues[A]] =
    lookupByType[A, ExtremeValues](typesToExtremeValues)

  implicit def mkFloatingPointValues[A: TypeTag]: Option[FloatingPointValues[A]] =
    lookupByType[A, FloatingPointValues](typesToFloatingPointValues)

  implicit def numeric[A](implicit g: Generic[A]): Numeric[A] =
    g.numeric.getOrElse(sys.error(s"$g has no associated numeric"))

  implicit def floatingPointValues[A](implicit g: Generic[A]): FloatingPointValues[A] =
    g.floatingPointValues.getOrElse(sys.error(s"$g has no associated floatingPointValues"))

  implicit def extremeValues[A](implicit g: Generic[A]): ExtremeValues[A] =
    g.extremeValues.getOrElse(sys.error(s"$g has no associated numeric values"))

  def zero[A: Generic]: A = numeric[A].zero
  def one[A: Generic]: A = numeric[A].one
  def number[A: Generic](value: Int): A = numeric[A].fromInt(value)

  def maxValue[A: Generic]: A = implicitly[ExtremeValues[A]].MaxValue
  def minValue[A: Generic]: A = implicitly[ExtremeValues[A]].MinValue
  def minPositiveValue[A: Generic]: A = implicitly[FloatingPointValues[A]].MinPositiveValue
  def positiveInfinity[A : Generic]: A = implicitly[FloatingPointValues[A]].PositiveInfinity
  def negativeInfinity[A : Generic]: A = implicitly[FloatingPointValues[A]].NegativeInfinity
  def NaN[A : Generic]: A = implicitly[FloatingPointValues[A]].NaN

  implicit def mkGenericOps[A: Generic](value: A): GenericOps[A] = new GenericOps[A](value)
}
