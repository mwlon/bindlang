package org.mwlon.bindlang

import org.mwlon.bindlang.SimpleAsts._
import org.mwlon.bindlang.Tokens._

import scala.collection.immutable.LazyList.#::
import scala.collection.mutable.ArrayBuffer

/*
Ops: Array[Op]  //global parsing starts here
Closure: { + Ops + }
Function Def: [bindable] + ( + Array[var + ,] + ) + Closure
While: while + Closure + Closure
For: for + ( + loopvar + , + n + ) + Closure
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
  case class ParseResult[+T <: SimpleAst](
    rest: LazyList[Token],
    ast: T
  )

  def parse(tokens: LazyList[Token]): SimpleAst = {
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

  private def parseOps(tokens: LazyList[Token]): ParseResult[ClosureAst] = {
    var rest = tokens
    val res = ArrayBuffer.empty[SimpleAst]
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

  private def parseOp(tokens: LazyList[Token]): Option[ParseResult[SimpleAst]] = {
    parseAlternatives(tokens, Seq(parseAssign, parseWhile, parseFor, parseExpression))
  }

  private def parseAssign(tokens: LazyList[Token]): Option[ParseResult[AssignAst]] = {
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

  private def parseWhile(tokens: LazyList[Token]): Option[ParseResult[WhileAst]] = {
    trimExactToken(tokens, While).map(theRest => {
      var rest = theRest
      val conditionParseResult = parseClosure(rest)
      rest = conditionParseResult.rest
      val effectParseResult = parseClosure(rest)
      rest = effectParseResult.rest
      ParseResult(rest, WhileAst(conditionParseResult.ast, effectParseResult.ast))
    })
  }

  private def parseFor(tokens: LazyList[Token]): Option[ParseResult[ForAst]] = {
    trimExactToken(tokens, For).map(theRest => {
      var rest = theRest
      rest = trimExactToken(rest, CharToken('(')).get
      val varName = rest match {
        case VarOrType(x) #:: rest2 =>
          rest = rest2
          x
        case _ => throw new Error("expected for loop variable as first arg")
      }
      rest = trimExactToken(rest, CharToken(',')).get
      val countResult = parseExpression(rest).get
      rest = countResult.rest
      rest = trimExactToken(rest, CharToken(')')).get
      val effectParseResult = parseClosure(rest)
      rest = effectParseResult.rest
      ParseResult(rest, ForAst(varName, countResult.ast, effectParseResult.ast))
    })
  }

  private def parseExpression(tokens: LazyList[Token]): Option[ParseResult[SimpleAst]] = {
    parseAlternatives(tokens, Seq(parseBindable, parseFunctionDef, parseUnaryBinary))
  }

  def parseBindable(tokens: LazyList[Token]): Option[ParseResult[FunctionDefAst]] = {
    trimExactToken(tokens, Bindable).map(rest => {
      val functionResult = parseFunctionDef(rest).get
      functionResult.ast.markBindable()
      functionResult
    })
  }

  def parseFunctionDef(tokens: LazyList[Token]): Option[ParseResult[FunctionDefAst]] = {
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

  def parseUnaryBinary(tokens: LazyList[Token]): Option[ParseResult[SimpleAst]] = {
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
        BinaryAst.from(unaries.toArray, binaryOps.toArray)
      )
    })
  }

  private def parseUnary(tokens: LazyList[Token]): Option[ParseResult[SimpleAst]] = {
    parseAlternatives(
      tokens,
      Seq(wrappedUnaryBinary, functionCall, unaryOpUnary, varUnary, constantUnary)
    )
  }

  private def wrappedUnaryBinary(tokens: LazyList[Token]): Option[ParseResult[SimpleAst]] = {
    trimExactToken(tokens, CharToken('(')).map(theRest => {
      var rest = theRest
      val result = parseExpression(tokens).get
      rest = trimExactToken(result.rest, CharToken(')')).get
      ParseResult(rest, result.ast)
    })
  }

  def functionCall(tokens: LazyList[Token]): Option[ParseResult[FunctionCallAst]] = {
    tokens match {
      case VarOrType(name) #:: _ =>
        trimExactToken(tokens.tail, CharToken('(')).map(theRest => {
          var rest = theRest
          val args = ArrayBuffer.empty[SimpleAst]
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

  private def unaryOpUnary(tokens: LazyList[Token]): Option[ParseResult[UnaryOpAst]] = {
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

  private def varUnary(tokens: LazyList[Token]): Option[ParseResult[VarAst]] = {
    tokens match {
      case VarOrType(name) #:: _ =>
        Some(ParseResult(
          tokens.tail,
          VarAst(name)
        ))
      case _ => None
    }
  }

  private def constantUnary(tokens: LazyList[Token]): Option[ParseResult[ConstantAst[_]]] = {
    tokens match {
      case (token: TypedToken[_]) #:: _ =>
        Some(ParseResult(
          tokens.tail,
          ConstantAst(token)
        ))
      case _ => None
    }
  }

  def parseClosure(tokens: LazyList[Token]): ParseResult[ClosureAst] = {
    var rest = trimExactToken(tokens, CharToken('{')).get
    val opsParsed = parseOps(rest)
    rest = opsParsed.rest
    rest = trimExactToken(rest, CharToken('}')).get
    ParseResult(rest, opsParsed.ast)
  }

  private def parseAlternatives[T <: SimpleAst](
    tokens: LazyList[Token],
    alternatives: Seq[LazyList[Token] => Option[ParseResult[T]]]
  ): Option[ParseResult[T]] = {
    var result: Option[ParseResult[T]] = None
    alternatives.takeWhile(alternative => {
      result = alternative(tokens)
      result.isEmpty
    })
    result
  }
}
