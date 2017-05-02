package nl.sogyo.calculator

import scala.util.{ Try, Success, Failure }

object Operator {
  def apply(x: String): Try[Operator] = x match {
    case "+" => Success(Plus)
    case "-" => Success(Minus)
    case "*" => Success(Times)
    case "/" => Success(Divide)
    case "^" => Success(Power)
    case _ => Failure(new exceptions.NotAnOperatorException)
  }
}

sealed trait Operator extends Token {
  def precedence: Int
  def isLeftAssociative: Boolean
}

sealed trait UnaryOperator extends Operator {
  def apply(a: Number): Number
}

sealed trait BinaryOperator extends Operator {
  def apply(a: Number, b: Number): Number
}

case object Plus extends BinaryOperator {
  val precedence = 2
  val isLeftAssociative = true;
  override def toString = "+"
  def apply(a: Number, b: Number): Number = Number(a.value + b.value)
}

case object Minus extends BinaryOperator {
  val precedence = 2
  val isLeftAssociative = true
  override def toString = "-"
  def apply(a: Number, b: Number): Number = Number(a.value - b.value)
}

case object Times extends BinaryOperator {
  val precedence = 3
  val isLeftAssociative = true
  override def toString = "*"
  def apply(a: Number, b: Number): Number = Number(a.value * b.value)
}

case object Divide extends BinaryOperator {
  val precedence = 3
  val isLeftAssociative = true
  override def toString = "/"
  def apply(a: Number, b: Number): Number = Number(a.value / b.value)
}

case object Power extends BinaryOperator {
  val precedence = 4
  val isLeftAssociative = false
  override def toString = "^"
  def apply(a: Number, b: Number): Number = Number(math.pow(a.value, b.value))
}