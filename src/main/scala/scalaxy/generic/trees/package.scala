package scalaxy.generic

package object trees extends GenericTreeSimplifiers with NumericTreeSimplifiers with TreeSimplifiers {
  override val global = scala.reflect.runtime.universe
  import global._

  override def treeSimplifiers = List(
    genericTreeSimplifier,
    numericTreeSimplifier)
}
