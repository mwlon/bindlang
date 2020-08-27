package org.mwlon.bindlang

import org.mwlon.bindlang.SimpleAsts.SimpleAst

import scala.collection.mutable.ListBuffer

case class Variable(name: String, rhs: SimpleAst) {
  val usedIn: ListBuffer[SimpleAst] = ListBuffer.empty

  def addUsage(ast: SimpleAst): Unit = {
    usedIn += ast
  }
}
