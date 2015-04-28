package scalaxy.generic.test

import scalaxy.generic.{ExtremeValues, FloatingPointValues, CommonValuesTreeSimplifiers}

import org.junit._
import org.junit.Assert._

import scala.language.implicitConversions
import scalaxy.generic.test.TestBase

class CommonValuesTreeSimplifiersTest
  extends TestBase
  with CommonValuesTreeSimplifiers
{
  import global._

  override def treeSimplifiers =
    List(commonValuesTreeSimplifier)

  @Test
  def testTreeRewrite {
    def numericOpTree[A : ExtremeValues : TypeTag]: Tree = {
      import Numeric.Implicits._
      reify({
        val max = implicitly[ExtremeValues[A]].MaxValue
        val min = implicitly[ExtremeValues[A]].MinValue
        val minPositiveValue = implicitly[FloatingPointValues[A]].MinPositiveValue
        val positiveInfinity = implicitly[FloatingPointValues[A]].PositiveInfinity
        val negativeInfinity = implicitly[FloatingPointValues[A]].NegativeInfinity
        val NaN = implicitly[FloatingPointValues[A]].NaN
      }).tree
    }

    val numericDoubleTree = numericOpTree[Double]
    val doubleTree = reify({
      val max = Double.MaxValue
      val min = Double.MinValue
      val minPositiveValue = Double.MinPositiveValue
      val positiveInfinity = Double.PositiveInfinity
      val negativeInfinity = Double.NegativeInfinity
      val NaN = Double.NaN
    }).tree
    assertEquals(typecheck(doubleTree).toString,
      simplifyGenericTree(typecheck(numericDoubleTree)).toString)
  }
}
