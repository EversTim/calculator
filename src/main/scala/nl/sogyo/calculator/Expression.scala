package nl.sogyo.calculator

import scala.util.{ Try, Success, Failure }

sealed trait Expression {
  def value: Try[Double]

  override def toString: String
}

trait BinaryOperator {
  def left: Expression
  def right: Expression

  override def toString = left.toString + " " + right.toString + " "
}

case class Function(func: (Double => Double), stringRepresentation: String, expr: Expression) extends Expression {
  def value = for (e <- expr.value) yield func(e)

  override def equals(that: Any): Boolean = that match {
    case f: Function => (this.stringRepresentation == f.stringRepresentation) && (this.expr == f.expr)
    case _ => false
  }
  
  override def toString = expr.toString + " " + stringRepresentation
}

case class Number(num: Double) extends Expression {
  val value = Success(num)
  override def toString = num.toString
}

case class Plus(left: Expression, right: Expression) extends Expression with BinaryOperator {
  def value = for (l <- left.value; r <- right.value) yield l + r
  override def toString = super.toString + "+"
}

case class Minus(left: Expression, right: Expression) extends Expression with BinaryOperator {
  def value = for (l <- left.value; r <- right.value) yield l - r
  override def toString = super.toString + "-"
}

case class Times(left: Expression, right: Expression) extends Expression with BinaryOperator {
  def value = for (l <- left.value; r <- right.value) yield l * r
  override def toString = super.toString + "*"
}

case class Divide(left: Expression, right: Expression) extends Expression with BinaryOperator {
  def value = for (l <- left.value; r <- right.value) yield l / r
  override def toString = super.toString + "/"
}

case class Power(left: Expression, right: Expression) extends Expression with BinaryOperator {
  def value = for (l <- left.value; r <- right.value) yield math.pow(l, r)
  override def toString = super.toString + "^"
}

case class Variable(name: String) extends Expression {
  def value = Failure(new exceptions.VariableValueException("A variable does not have a value!"))
  
  override def toString = name
}

case object Empty extends Expression {
  def value = Failure(new exceptions.EmptyExpressionValueException("An empty expression has no value!"))
  override def toString = "EMPTY"
}