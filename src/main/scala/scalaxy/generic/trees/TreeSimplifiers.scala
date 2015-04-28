package scalaxy.generic

import scala.reflect.api.Universe

import scala.reflect.NameTransformer

private[generic]
trait TreeSimplifiers extends Utils
{
  val global: Universe
  import global._

  type TreeSimplifier =
    PartialFunction[(Tree, Tree => Tree), Tree]

  def typecheck(tree: Tree): Tree

  def treeSimplifiers: List[TreeSimplifier]

  private[this] lazy val treeSimplifier: Tree => Tree = {
    val f: TreeSimplifier =
      treeSimplifiers.reduceLeft(_ orElse _)

    val transformer = new Transformer {
      val self = (tree: Tree) => transform(tree)

      override def transform(tree: Tree): Tree = {
        val sup = (tt: (Tree, Tree => Tree)) => super.transform(tt._1)
        f.applyOrElse((tree, self), sup) match {
          case tree @ q"${target @ WithType(tpe1)}.asInstanceOf[$tpe2]: $tpe3"
              if tpe1 != NoType && tpe1 =:= tpe2.tpe && tpe1 =:= tpe3.tpe =>
            target

          case tree =>
            tree
        }
      }
    }
    transformer.transform(_)
  }

  def simplifyGenericTree(tree: Tree): Tree = treeSimplifier(tree)
}
