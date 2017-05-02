package nl.sogyo.calculator

import scala.util.{ Try, Success, Failure }

object Number {
  def apply(x: String): Try[Number] = try {
    Success(Number(x.toDouble))
  } catch {
    case nfe: NumberFormatException => Failure(new exceptions.NotANumberException(x))
  }
}

case class Number(value: Double) extends Token {
  override def toString = value.toString
}