package nl.sogyo.calculator

import scala.util.{ Try, Success, Failure }

sealed trait Expression {
  def value: Try[Double] = value(Map())

  def value(vars: Map[Variable, Expression] = Map()): Try[Double]

  override def toString: String
}

object Expression {
  def apply(str: String): Try[Expression] = ExpressionParser(str)
}

trait BinaryOperator {
  def left: Expression
  def right: Expression
  def func: (Double, Double) => Double

  override def toString = left.toString + " " + right.toString + " "
}

trait CommutativeOperator

case class Function(func: (Double => Double), stringRepresentation: String, expr: Expression) extends Expression {
  def value(vars: Map[Variable, Expression]): Try[Double] = for (e <- expr.value(vars)) yield func(e)

  override def equals(that: Any): Boolean = that match {
    case f: Function => (this.stringRepresentation == f.stringRepresentation) && (this.expr == f.expr)
    case _ => false
  }

  override def toString = expr.toString + " " + stringRepresentation
}

case class Number(num: Double) extends Expression {
  def value(vars: Map[Variable, Expression]): Try[Double] = Success(num)
  override def toString = num.toString

  def optimize: Expression = this
}

case class Plus(left: Expression, right: Expression) extends Expression with BinaryOperator with CommutativeOperator {
  def value(vars: Map[Variable, Expression]): Try[Double] = for (l <- left.value(vars); r <- right.value(vars)) yield func(l, r)

  def func: (Double, Double) => Double = (x, y) => x + y

  override def toString = super.toString + "+"
}

case class Minus(left: Expression, right: Expression) extends Expression with BinaryOperator {
  def value(vars: Map[Variable, Expression]): Try[Double] = for (l <- left.value(vars); r <- right.value(vars)) yield func(l, r)

  def func: (Double, Double) => Double = (x, y) => x - y

  override def toString = super.toString + "-"
}

case class Multiply(left: Expression, right: Expression) extends Expression with BinaryOperator {
  def value(vars: Map[Variable, Expression]): Try[Double] = for (l <- left.value(vars); r <- right.value(vars)) yield func(l, r)

  def func: (Double, Double) => Double = (x, y) => x * y

  override def toString = super.toString + "*"
}

case class Divide(left: Expression, right: Expression) extends Expression with BinaryOperator {
  def value(vars: Map[Variable, Expression]): Try[Double] = for (l <- left.value(vars); r <- right.value(vars)) yield func(l, r)

  def func: (Double, Double) => Double = (x, y) => x / y

  override def toString = super.toString + "/"
}

case class Power(left: Expression, right: Expression) extends Expression with BinaryOperator {
  def value(vars: Map[Variable, Expression]): Try[Double] = for (l <- left.value(vars); r <- right.value(vars)) yield func(l, r)

  override def toString = super.toString + "^"

  def func: (Double, Double) => Double = (x, y) => math.pow(x, y)
}

case class Variable(name: String) extends Expression {
  def value(vars: Map[Variable, Expression]): Try[Double] = vars.get(this) match {
    case None => Failure(new exceptions.VariableValueException("No value defined for variable " + name + "!"))
    case Some(e) => e.value(vars - this)
  }

  override def toString = name
}

case object Empty extends Expression {
  def value(vars: Map[Variable, Expression]): Try[Double] = Failure(new exceptions.EmptyExpressionValueException("An empty expression has no value!"))
  override def toString = "EMPTY"
}