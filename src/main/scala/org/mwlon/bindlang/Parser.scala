package org.mwlon.bindlang

import org.mwlon.bindlang.Asts.{AssignAst, Ast, ClosureAst, ConstantAst, FunctionCallAst, FunctionDefAst, GlobalAst, UnaryBinaryAst, UnaryOpAst, VarAst, WhileAst}
import org.mwlon.bindlang.Tokens.{BinaryOp, CharToken, Token, TypedToken, UnaryOp, VarOrType, While}

import scala.collection.immutable.LazyList.#::
import scala.collection.mutable.ArrayBuffer

/*
Ops: Array[Op]  //global parsing starts here
Closure: { + Ops + }
Function Def: ( + Array[var + ,] + ) + Closure
While: while + Closure + Closure
If: if + Closure + Closure [+ else + Closure]
Op
  Assign: Var + = + Expression
  Expression
  While
Expression
  Function Def
  UnaryBinary: Unary + Array[binary op + Unary]
Unary
  ( + UnaryBinary + )
  Function Call: Var + ( + Array[Expression + ,] + )
  unary op + Unary
  Method: Unary + method call + ( + Array[Expression + ,] + )
  Constant: typed token
  Variable: var
 */

object Parser {
  case class ParseResult(
    rest: LazyList[Token],
    ast: Ast
  )

  def parse(tokens: LazyList[Token]): Ast = {
    val result = parseOps(tokens)
    assert(result.rest.isEmpty, s"training tokens: ${result.rest.toArray.mkString(" ")}")
    GlobalAst(result.ast.children)
  }

  private def trimExactToken(tokens: LazyList[Token], expected: Token): Option[LazyList[Token]] = {
    tokens match {
      case x #:: _ if x == expected => Some(tokens.tail)
      case _ => None
    }
  }

  private def parseOps(tokens: LazyList[Token]): ParseResult = {

    var rest = tokens
    val res = ArrayBuffer.empty[Ast]
    var good = true
    while (good) {
      val opResult = parseOp(rest)
      if (opResult.isDefined) {
        val gotten = opResult.get
        res += gotten.ast
        rest = gotten.rest
      } else {
        good = false
      }
    }
    ParseResult(
      rest,
      ClosureAst(res.toArray)
    )
  }

  private def parseOp(tokens: LazyList[Token]): Option[ParseResult] = {
    parseAlternatives(tokens, Array(parseAssign, parseWhile, parseExpression))
  }

  private def parseAssign(tokens: LazyList[Token]): Option[ParseResult] = {
    tokens match {
      case head #:: CharToken('=') #:: rest =>
        head match {
          case VarOrType(name) =>
            val exprResult = parseExpression(rest).get
            Some(ParseResult(
              exprResult.rest,
              AssignAst(name, exprResult.ast)
            ))
          case _ => throw new Error("Cannot assign to non-var")
        }
      case _ => None
    }
  }

  private def parseWhile(tokens: LazyList[Token]): Option[ParseResult] = {
    trimExactToken(tokens, While).map(theRest => {
      var rest = theRest
      val conditionParseResult = parseClosure(rest)
      rest = conditionParseResult.rest
      val effectParseResult = parseClosure(rest)
      rest = effectParseResult.rest
      ParseResult(rest, WhileAst(conditionParseResult.ast, effectParseResult.ast))
    })
  }

  private def parseExpression(tokens: LazyList[Token]): Option[ParseResult] = {
    parseAlternatives(tokens, Array(parseFunctionDef, parseUnaryBinary))
  }

  def parseFunctionDef(tokens: LazyList[Token]): Option[ParseResult] = {
    trimExactToken(tokens, CharToken('(')).map(theRest => {
      var rest = theRest
      var good = true
      val newVars = ArrayBuffer.empty[String]
      while (good) {
        rest match {
          case VarOrType(name) #:: CharToken(')') #:: _ =>
            newVars += name
            rest = rest.tail
          case VarOrType(name) #:: CharToken(',') #:: rest2 =>
            newVars += name
            rest = rest2
          case _ =>
            good = false
        }
      }
      rest = trimExactToken(rest, CharToken(')')).get
      val closureResult = parseClosure(rest)
      ParseResult(closureResult.rest, FunctionDefAst(newVars.toArray, closureResult.ast))
    })
  }

  def parseUnaryBinary(tokens: LazyList[Token]): Option[ParseResult] = {
    parseUnary(tokens).map(firstUnaryResult => {
      var rest = firstUnaryResult.rest

      val unaries = ArrayBuffer(firstUnaryResult.ast)
      val binaryOps = ArrayBuffer.empty[String]
      var good = true
      while (good) {
        rest match {
          case (x: BinaryOp) #:: _ =>
            val parsedResult = parseUnary(rest.tail).get
            binaryOps += x.name
            unaries += parsedResult.ast
            rest = parsedResult.rest
          case _ => good = false
        }
      }

      ParseResult(
        rest,
        UnaryBinaryAst.from(unaries.toArray, binaryOps.toArray)
      )
    })
  }

  private def parseUnary(tokens: LazyList[Token]): Option[ParseResult] = {

    parseAlternatives(
      tokens,
      Array(wrappedUnaryBinary, functionCall, unaryOpUnary, varUnary, constantUnary)
    )
  }

  private def wrappedUnaryBinary(tokens: LazyList[Token]): Option[ParseResult] = {

    trimExactToken(tokens, CharToken('(')).map(theRest => {
      var rest = theRest
      val result = parseExpression(tokens).get
      rest = trimExactToken(result.rest, CharToken(')')).get
      ParseResult(rest, result.ast)
    })
  }

  def functionCall(tokens: LazyList[Token]): Option[ParseResult] = {

    tokens match {
      case VarOrType(name) #:: _ =>
        trimExactToken(tokens.tail, CharToken('(')).map(theRest => {
          var rest = theRest
          val args = ArrayBuffer.empty[Ast]
          var good = true
          while (good) {
            val argResult = parseExpression(rest)
            if (argResult.isDefined) {
              val result = argResult.get
              args += result.ast
              result.rest match {
                case CharToken(')') #:: _ =>
                  rest = result.rest
                  good = false
                case CharToken(',') #:: CharToken(')') #:: _ =>
                  rest = result.rest.tail
                  good = false
                case CharToken(',') #:: _ =>
                  rest = result.rest.tail
                case _ =>
                  throw new Error("Not a function call arg")
              }
            } else {
              good = false
            }
          }
          rest = trimExactToken(rest, CharToken(')')).get
          ParseResult(
            rest,
            FunctionCallAst(name, args.toArray)
          )
        })
      case _ => None
    }
  }

  private def unaryOpUnary(tokens: LazyList[Token]): Option[ParseResult] = {

    tokens match {
      case UnaryOp(name) #:: _ =>
        val term = parseUnary(tokens.tail).get
        Some(ParseResult(
          term.rest,
          UnaryOpAst(name, term.ast)
        ))
      case _ => None
    }
  }

  private def varUnary(tokens: LazyList[Token]): Option[ParseResult] = {

    tokens match {
      case VarOrType(name) #:: _ =>
        Some(ParseResult(
          tokens.tail,
          VarAst(name)
        ))
      case _ => None
    }
  }

  private def constantUnary(tokens: LazyList[Token]): Option[ParseResult] = {

    tokens match {
      case (token: TypedToken[_]) #:: _ =>
        Some(ParseResult(
          tokens.tail,
          ConstantAst(token)
        ))
      case _ => None
    }
  }

  def parseClosure(tokens: LazyList[Token]): ParseResult = {

    var rest = trimExactToken(tokens, CharToken('{')).get
    val opsParsed = parseOps(rest)
    rest = opsParsed.rest
    rest = trimExactToken(rest, CharToken('}')).get
    ParseResult(rest, opsParsed.ast)
  }

  private def parseAlternatives(tokens: LazyList[Token], alternatives: Array[LazyList[Token] => Option[ParseResult]]): Option[ParseResult] = {
    var result: Option[ParseResult] = None
    var i = 0
    while (result.isEmpty && i < alternatives.length) {
      result = alternatives(i)(tokens)
      if (result.isDefined) {

      }
      i += 1
    }
    if (result.isEmpty) {

    }
    result
  }
}
