package org.mwlon.bindlang

import scala.io.Source

//compiler for bindlang -> jar
object Main {
  def main(args: Array[String]): Unit = {
    val f = args(0)
    println(s"parsing $f")
    Compiler.compile(f, "out.jar")
  }
}
