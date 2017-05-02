package nl.sogyo.calculator

import scala.util.{ Try, Success, Failure }

trait Token {
  override def toString: String
}

object Token {
  def apply(x: String): Try[Token] = Operator(x) match {
    case Success(a) => Success(a)
    case Failure(_) => Number(x) match {
      case Success(b) => Success(b)
      case Failure(_) => Parens(x) match {
        case Success(c) => Success(c)
        case Failure(_) => Failure(new exceptions.NotATokenException(x))
      }
    }
  }
}