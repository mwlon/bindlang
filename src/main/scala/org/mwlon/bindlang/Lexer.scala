package org.mwlon.bindlang

import org.mwlon.bindlang.Tokens._

import scala.collection.mutable
import scala.collection.immutable.LazyList.#::

object Lexer {
  private val letters: Set[Char] = {
    val res = mutable.Set.empty[Char]
    for (i <- 65 until 91) {
      res += i.toChar
    }
    for (i <- 97 until 123) {
      res += i.toChar
    }
    res.toSet
  }
  private val numbers: Set[Char] = {
    val res = mutable.Set.empty[Char]
    for (i <- 48 until 58) {
      res += i.toChar
    }
    res.toSet
  }
  private val specialChars: Set[Char] = Set(
    '{',
    '}',
    '(',
    ')',
    '[',
    ']',
    ':',
    ','
  )
  private val whitespace: Set[Char] = Set(
    ' ',
    '\t',
    '\r',
    '\n'
  )
  private val binaryOpChars: Set[Char] = Set(
    '%',
    '^',
    '&',
    '*',
    '|',
    '<',
    '>',
    '/',
    '-',
    '+',
    '='
  )
  private val unaryOpChars: Set[Char] = Set(
    '~',
    '!',
    '@'
  )

  private case class StateTransition(
    alternative: State,
  )

  private val baseCharMap: Map[Char, State] = {
    val res = mutable.Map.empty[Char, State]
    for (char <- letters) {
      res(char) = TextState
    }
    for (char <- numbers) {
      res(char) = NumberState
    }
    for (char <- binaryOpChars) {
      res(char) = BinaryOpState
    }
    for (char <- unaryOpChars) {
      res(char) = UnaryOpState
    }
    for (char <- whitespace) {
      res(char) = WhitespaceState
    }
    for (char <- specialChars) {
      res(char) = SingleCharState
    }
    res('.') = MethodState
    res.toMap
  }

  private trait State {
    val continuationChars: Set[Char] = Set()
    def accept(char: Char): Option[StateTransition] = {
      if (continuationChars(char)) {
        None
      } else {
        Some(StateTransition(baseCharMap(char)))
      }
    }
    def emit(str: String): Option[Token] = None
  }

  private object WhitespaceState extends State {
    override val continuationChars: Set[Char] = whitespace
  }

  private object TextState extends State {
    override val continuationChars: Set[Char] = letters ++ numbers

    override def emit(str: String): Option[Token] = {
      val res = str match {
        case "if" => Tokens.If
        case "else" => Tokens.Else
        case "while" => Tokens.While
        case "for" => Tokens.For
        case "bindable" => Tokens.Bindable
        case _ => Tokens.VarOrType(str)
      }
      Some(res)
    }
  }

  private object NumberState extends State {
    override val continuationChars: Set[Char] = numbers + '.'

    override def emit(str: String): Option[Token] = {
      val numDots = str.count(_ == '.')
      if (numDots == 0) {
        Some(LongToken(str.toLong))
      } else if (numDots == 1) {
        Some(DoubleToken(str.toDouble))
      } else {
        throw new Error(s"invalid number: $str")
      }
    }
  }

  private object MethodState extends State {
    override def accept(char: Char): Option[StateTransition] = {
      Some(StateTransition(MethodNameState))
    }
  }

  private object MethodNameState extends State {
    override val continuationChars: Set[Char] = letters ++ numbers

    override def emit(str: String): Option[Token] = {
      if (str.length == 0) {
        throw new Error("missing method name")
      }
      Some(MethodToken(str))
    }
  }

  private object BinaryOpState extends State {
    override val continuationChars: Set[Char] = binaryOpChars

    override def emit(str: String): Option[Token] = {
      val res = str match {
        case "=" => CharToken('=')
        case _ => BinaryOp(str)
      }
      Some(res)
    }
  }

  private object UnaryOpState extends State {
    override val continuationChars: Set[Char] = unaryOpChars

    override def emit(str: String): Option[Token] = {
      Some(UnaryOp(str))
    }
  }

  private object SingleCharState extends State {
    override val continuationChars: Set[Char] = Set()

    override def emit(str: String): Option[Token] = {
      Some(CharToken(str(0)))
    }
  }

  def lex(stream: LazyList[Char]): LazyList[Token] = {
    var state: State = baseCharMap(stream.head)
    var start = 0
    var end = 0

    //add one whitespace char at end to make sure final transition is reached
    (stream #::: (' ' #:: LazyList.empty)).flatMap(char => {
      val maybeTransition = state.accept(char)
      val res = if (maybeTransition.isEmpty) {
        Seq()
      } else {
        val str = stream.slice(start, end).map(_.toString).mkString("")
        val token = state.emit(str)
        start = end
        state = maybeTransition.get.alternative
        token
      }
      end += 1
      res
    })
  }

}
