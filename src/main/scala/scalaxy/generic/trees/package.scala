package scalaxy.generic

import scala.reflect.runtime.universe._

package object trees extends GenericTrees with NumericTrees with TreeSimplification {
  override val global = scala.reflect.runtime.universe
  import global._

  override def treeSimplifiers = List(
    genericTreeSimplifier,
    numericTreeSimplifier)
}
