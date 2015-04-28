package scalaxy.generic

private[generic] trait CommonValuesTreeSimplifiers extends TreeSimplifiers with Utils {
  val global: scala.reflect.api.Universe
  import global._

  private[this] lazy val commonValueSymbols: Set[Symbol] =
    Set("scalaxy.generic.ExtremeValues", "scalaxy.generic.FloatingPointValues")
      .map(rootMirror.staticClass(_))

  def commonValuesTreeSimplifier: TreeSimplifier = {
    case (Select(WithType(tpe @ ConcreteType()), n: TermName), transform)
        if commonValueSymbols(tpe.typeSymbol) =>
      val TypeRef(_, _, List(tparam)) = tpe
      typecheck(q"${tparam.typeSymbol.companion}.$n")
  }
}
