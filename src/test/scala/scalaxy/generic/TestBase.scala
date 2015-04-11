package scalaxy.generic.test

import scala.reflect.runtime.currentMirror
import scala.tools.reflect.ToolBox

trait TestBase {

  val global = scala.reflect.runtime.universe
  import global._

  private[this] lazy val toolbox =
    currentMirror.mkToolBox()

  def typecheck(t: Tree): Tree =
    toolbox.typecheck(t.asInstanceOf[toolbox.u.Tree]).asInstanceOf[Tree]
}
