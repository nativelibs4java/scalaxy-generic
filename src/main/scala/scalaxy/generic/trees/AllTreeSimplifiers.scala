package scalaxy.generic

trait AllTreeSimplifiers extends GenericTreeSimplifiers with NumericTreeSimplifiers {
  val global: scala.reflect.api.Universe
  import global._

  override def treeSimplifiers = List(
    genericTreeSimplifier,
    numericTreeSimplifier)
}
