package scalaxy.generic
package test

import org.junit._
import org.junit.Assert._

import scala.language.implicitConversions

class GenericTreesTest
  extends TestBase
  with GenericTrees
  with TreeSimplification
{
  import global._

  override def treeSimplifiers =
    List(genericTreeSimplifier)

  @Test
  def testTreeRewrite {
    def genericOpTree[A: Generic: TypeTag]: Tree = {
      reify({
        var a = one[A]
        a = a + number[A](10)
        a = a * number[A](2)
        a = a / number[A](3)
        a.toDouble
      }).tree
    }
    val genericDoubleTree = genericOpTree[Double]
    val doubleTree = reify({
      var a = 1.0
      a = a + 10.0
      a = a * 2.0
      a = a / 3.0
      a.toDouble
    }).tree
    assertEquals(
      typecheck(doubleTree).toString,
      simplifyGenericTree(typecheck(genericDoubleTree)).toString
        .replaceAll("\\$times", "*")
        .replaceAll("\\$plus", "+")
        .replaceAll("\\$minus", "-")
        .replaceAll("\\$div", "/"))
  }
}
