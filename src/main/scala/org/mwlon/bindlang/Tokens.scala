package org.mwlon.bindlang

object Tokens {
  trait Token extends Serializable
  trait TypedToken[T] extends Token {
    val value: T
  }

  case object Class extends Token
  case object If extends Token
  case object Else extends Token
  case object While extends Token
  case class VarOrType(name: String) extends Token
  case class CharToken(char: Char) extends Token
  case class BinaryOp(name: String) extends Token
  case class UnaryOp(name: String) extends Token
  case class MethodToken(name: String) extends Token
  case class DoubleToken(value: Double) extends TypedToken[Double]
  case class LongToken(value: Long) extends TypedToken[Long]
}
