package org.mwlon.bindlang

import org.mwlon.bindlang.Tokens.{BinaryOp, CharToken, UnaryOp, VarOrType}

import collection.immutable.LazyList.#::

class ParserTest extends TestBase {
  "functionCall" should "do the good stuff" in {
    val stream = VarOrType("f") #::
      CharToken('(') #::
      VarOrType("a") #::
      BinaryOp("+") #::
      VarOrType("b") #::
      CharToken(',') #::
      CharToken(')') #::
      CharToken(',') #::
      LazyList.empty

    val res = Parser.functionCall(stream).get
    res.rest.toArray match {
      case Array(CharToken(',')) =>
      case _ => throw new AssertionError(s"rest of ${res.rest.toString()} did not match expected")
    }
  }

  "functionDef" should "do the good stuff" in {
    val stream = CharToken('(') #::
      VarOrType("a") #::
      CharToken(',') #::
      VarOrType("b") #::
      CharToken(',') #::
      CharToken(')') #::
      CharToken('{') #::
      VarOrType("a") #::
      BinaryOp("+") #::
      VarOrType("b") #::
      CharToken('}') #::
      CharToken(',') #::
      LazyList.empty

    val res = Parser.parseFunctionDef(stream).get
    res.rest.toArray match {
      case Array(CharToken(',')) =>
      case _ => throw new AssertionError(s"rest of ${res.rest.toString()} did not match expected")
    }
  }

  "closure" should "do the good stuff" in {
    val stream =
      CharToken('{') #::
      VarOrType("a") #::
      BinaryOp("+") #::
      VarOrType("b") #::
      CharToken('}') #::
      CharToken(',') #::
      LazyList.empty

    val res = Parser.parseClosure(stream)
    res.rest.toArray match {
      case Array(CharToken(',')) =>
      case _ => throw new AssertionError(s"rest of ${res.rest.toString()} did not match expected")
    }
  }

  "unaryBinary" should "do the good stuff" in {
    val stream = VarOrType("a") #::
      BinaryOp("+") #::
      UnaryOp("~") #::
      VarOrType("b") #::
      CharToken('}') #::
      LazyList.empty

    val res = Parser.parseUnaryBinary(stream)
    res.rest.toArray match {
      case Array(CharToken('}')) =>
      case _ => throw new AssertionError(s"rest of ${res.rest.toString()} did not match expected")
    }
  }
}
