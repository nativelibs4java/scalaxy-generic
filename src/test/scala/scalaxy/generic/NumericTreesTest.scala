package scalaxy.generic.test

import scalaxy.generic.NumericTreeSimplifiers

import org.junit._
import org.junit.Assert._

import scala.language.implicitConversions

class NumericTreeSimplifiersTest
  extends TestBase
  with NumericTreeSimplifiers
{
  import global._

  override def treeSimplifiers =
    List(numericTreeSimplifier)

  @Test
  def testTreeRewrite {
    def numericOpTree[A: Numeric: TypeTag]: Tree = {
      import Numeric.Implicits._
      reify({
        var a = implicitly[Numeric[A]].one
        a = a + implicitly[Numeric[A]].fromInt(10)
        a = a * implicitly[Numeric[A]].fromInt(2)
        // a = a / implicitly[Numeric[A]].fromInt(3)
        a.toDouble
      }).tree
    }
    val numericDoubleTree = numericOpTree[Double]
    val doubleTree = reify({
      var a = 1.0
      a = a + (10.0)
      a = a * (2.0)
      // a = (a / 3.0)
      a.toDouble
    }).tree
    assertEquals(typecheck(doubleTree).toString,
      simplifyGenericTree(typecheck(numericDoubleTree)).toString
        .replaceAll("\\$times", "*")
        .replaceAll("\\$plus", "+")
        .replaceAll("\\$minus", "-")
        .replaceAll("\\$div", "-"))
  }
}
