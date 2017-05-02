package nl.sogyo.calculator

import scala.util.{ Try, Success, Failure }

sealed trait Parens extends Token

object Parens {
  def apply(x: String): Try[Parens] = x match {
    case "(" => Success(LeftParens)
    case ")" => Success(RightParens)
    case _ => Failure(new exceptions.NotAParensException)
  }
}

case object LeftParens extends Parens {
  override def toString = "("
}

case object RightParens extends Parens {
  override def toString = ")"
}