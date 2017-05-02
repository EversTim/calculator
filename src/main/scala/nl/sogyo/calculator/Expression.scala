package nl.sogyo.calculator

import scala.util.{ Try, Success, Failure }

sealed trait Expression {
  def value: Try[Double]
  override def toString: String = this match {
    case Number(num) => num.toString
    case b: BinaryOperator => b.left + " " + b.right + " " + (b match {
      case _: Plus => "+"
      case _: Minus => "-"
      case _: Times => "*"
      case _: Divide => "/"
      case _: Power => "^"
    })
    case _ => ""
  }
}

trait BinaryOperator {
  def left: Expression
  def right: Expression
}

case class Number(num: Double) extends Expression {
  val value = Success(num)
}

case class Plus(left: Expression, right: Expression) extends Expression with BinaryOperator {
  def value = for (l <- left.value; r <- right.value) yield l + r
}

case class Minus(left: Expression, right: Expression) extends Expression with BinaryOperator {
  def value = for (l <- left.value; r <- right.value) yield l - r
}

case class Times(left: Expression, right: Expression) extends Expression with BinaryOperator {
  def value = for (l <- left.value; r <- right.value) yield l * r
}

case class Divide(left: Expression, right: Expression) extends Expression with BinaryOperator {
  def value = for (l <- left.value; r <- right.value) yield l / r
}

case class Power(left: Expression, right: Expression) extends Expression with BinaryOperator {
  def value = for (l <- left.value; r <- right.value) yield math.pow(l, r)
}

case object Empty extends Expression {
  def value = Failure(new exceptions.EmptyExpressionValueException("An empty expression has no value!"))
}