package scalaxy.generic

import scala.reflect.api.Universe

import scala.reflect.NameTransformer
import scala.language.implicitConversions

private[generic] trait GenericTreeSimplifiers extends TreeSimplifiers with Utils {
  val global: Universe
  import global._

  private lazy val genericPackageSymbol = rootMirror.staticModule("scalaxy.generic.package")

  private object StringConstant {
    def unapply(tree: Tree) = Option(tree) collect {
      case Literal(Constant(s: String)) => s
    }
  }
  private object GenericPackage {
    def unapply(tree: Tree): Boolean = tree.symbol == genericPackageSymbol
  }
  private object GenericOpsCreation {
    def isGenericOp(tree: Tree) =
      tree.tpe != null && tree.tpe <:< typeOf[GenericOps[_]]

    def unapply(tree: Tree): Option[(Tree, Type)] = {
      Option(tree).filter(isGenericOp) collect {
        case q"$pack.mkGenericOps[$tpt]($value)($evidence)" if pack.symbol == genericPackageSymbol =>
          value -> tpt.tpe
      }
    }
  }
  private object GenericOpsCall {
    def unapply(tree: Tree): Option[(Tree, Type, String, TermName)] = Option(tree) collect {

      case Apply(Select(GenericOpsCreation(target, tpe), callName), List(StringConstant(methodName))) =>
        (target, tpe, callName.toString, TermName(methodName))
    }
  }
  private object GenericNumberCall {
    def unapply(tree: Tree): Option[(Int, Type)] = Option(tree) collect {

      case Apply(TypeApply(Select(GenericPackage(), N(n @ ("one" | "zero"))), List(tpt)), List(ev)) =>
        (if (n == "one") 1 else 0, tpt.tpe)

      case Apply(Apply(TypeApply(Select(GenericPackage(), N("number")), List(tpt)), List(Literal(Constant(n: Int)))), List(ev)) =>
        (n, tpt.tpe)
    }
  }

  private def normalizeName(name: String): TermName =
    TermName(NameTransformer.encode(NameTransformer.decode(name)))

  private def restrictMethodSymbol(sym: Symbol, argTypes: List[Type]): Symbol = {
    sym.suchThat(s => {
      s.isMethod && {
        val params = s.asMethod.paramLists.flatten

        params.size == argTypes.size && params.zip(argTypes).forall {
          case (param, argType) =>
            argType == null ||
            argType == NoType ||
            argType <:< param.typeSignature
        }
      }
    })
  }

  def genericTreeSimplifier: TreeSimplifier = {

    case (q"${target @ WithType(t1)}.asInstanceOf[${WithType(t2)}]", transformer)
        if t1 =:= t2 =>
      transformer(target)

    case (GenericNumberCall(n, tpe), transformer) if typesToNumerics.contains(tpe) =>
      Literal(Constant(typesToNumerics(tpe).fromInt(n)))

    case (Apply(GenericOpsCall(target, tpe @ ConcreteType(), "applyDynamic", methodName), args), transformer) =>
      val sym = restrictMethodSymbol(tpe member methodName, args.map(_.tpe))
      q"${transformer(target)}.$sym(..${args.map(transformer)})"

    case (Apply(GenericOpsCall(target, tpe @ ConcreteType(), "updateDynamic", methodName), List(value)), transformer) =>
      val sym = restrictMethodSymbol(tpe member TermName("update"), List(value.tpe))
      q"${transformer(target)}.$sym(${transformer(value)})"

    case (GenericOpsCall(target, tpe @ ConcreteType(), "selectDynamic", methodName), transformer) =>
      val sym = restrictMethodSymbol(tpe member methodName, Nil)
      q"${transformer(target)}.$sym"
  }
}
