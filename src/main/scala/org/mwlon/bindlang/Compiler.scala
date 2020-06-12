package org.mwlon.bindlang

import scala.io.{BufferedSource, Source}

object Compiler {
  def compile(inputFile: String, outputFilename: String): Unit = {
    val source = Source.fromFile(inputFile)
    val charStream = source.to(LazyList)
    val tokenStream = Lexer.lex(charStream)
    tokenStream.foreach(token => println(token))
    println("now...")
    val ast = Parser.parse(tokenStream)
    println(ast.toLlvmIr)
  }
}
