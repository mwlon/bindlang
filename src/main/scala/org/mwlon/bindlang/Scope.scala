package org.mwlon.bindlang

import scala.collection.mutable

case class Scope(vars: mutable.Map[String, Variable] = mutable.Map.empty) {
  def contains(varname: String): Boolean = vars.contains(varname)

  def get(varname: String): Option[Variable] = {
    vars.get(varname)
  }

  def add(v: Variable): Unit = {
    vars(v.name) = v
  }
}
