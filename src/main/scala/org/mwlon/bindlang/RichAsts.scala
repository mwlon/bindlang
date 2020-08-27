package org.mwlon.bindlang

import org.mwlon.bindlang.SimpleAsts.GlobalAst

object RichAsts {
  trait RichAst {
    val parent: Option[RichAst]
    val children: Array[RichAst]
    val scope: Scope
  }

  def build(globalAst: GlobalAst): RichClosure = {
    val scope = Scope()
    val children = globalAst.children.map
    RichClosure()
  }

  case class RichClosure(children: Array[RichAst], scope: Scope) extends RichAst {
    override val parent: Option[RichAst] = None
  }
}
