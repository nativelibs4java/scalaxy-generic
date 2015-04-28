package scalaxy.generic

trait AllTreeSimplifiers
    extends GenericTreeSimplifiers
    with NumericTreeSimplifiers
    with CommonValuesTreeSimplifiers {
  val global: scala.reflect.api.Universe
  import global._

  override def treeSimplifiers = List(
    commonValuesTreeSimplifier,
    genericTreeSimplifier,
    numericTreeSimplifier)
}
