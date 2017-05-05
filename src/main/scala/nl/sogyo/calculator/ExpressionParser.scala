package nl.sogyo.calculator

import scala.util.parsing.combinator._

import scala.util.Try

trait ExpressionParser extends RegexParsers {
  def literals: Map[String, Double] = Map("PI" -> math.Pi, "E" -> math.E)
  def functions: Map[String, Double => Double] = Map("sin" -> math.sin, "cos" -> math.cos, "tan" -> math.tan, "exp" -> math.exp)

  def expr = plusMinus

  def numberWithIntegerPart: Parser[Number] = """(-?[0-9]+(\.[0-9]+)?)""".r ^^ { x => Number(x.toDouble) }

  def numberWithOnlyFractionalPart: Parser[Number] = """-?.[0-9]+""".r ^^ { x => Number(x.toDouble) }

  def number: Parser[Expression] = numberWithIntegerPart | numberWithOnlyFractionalPart

  def literal: Parser[Number] = """[A-Z]+""".r ^^ { x => Number(literals(x)) }

  def parens: Parser[Expression] = ("(" ~> plusMinus <~ ")")

  def function: Parser[Function] = ("""[a-z]{2,}\(""".r ~ plusMinus <~ ")") ^^ { case str ~ expr => Function(functions(str.init), str.init, expr) }

  def variable: Parser[Variable] = """[a-z]""".r ^^ (Variable(_))

  def text = function | literal | parens | variable

  def numberLike = literal | number | variable

  def plusMinus: Parser[Expression] = multiplyDivide ~ rep("+" ~ multiplyDivide | "-" ~ multiplyDivide) ^^ {
    case num ~ list => list.foldLeft(num) {
      case (x, "+" ~ y) => Plus(x, y)
      case (x, "-" ~ y) => Minus(x, y)
    }
  }

  def multiplyDivide: Parser[Expression] = multiplyDivideExplicitAndImplicitRight

  def multiplyDivideExplicitAndImplicitRight: Parser[Expression] = power ~ rep("*" ~ power | "/" ~ power | "" ~ text) ^^ {
    case num ~ list => list.foldLeft(num) {
      case (x, "*" ~ y) => Multiply(x, y)
      case (x, "/" ~ y) => Divide(x, y)
      case (x, "" ~ y) => Multiply(x, y)
    }
  }

  def power: Parser[Expression] = rep(recurse <~ "^") ~ recurse ^^ {
    case list ~ num => list.foldRight(num) {
      case (x, y) => Power(x, y)
    }
  }

  def recurse = text | number
}

object ExpressionParser extends ExpressionParser {
  def apply(input: String): Try[Expression] = {
    parseAll(expr, input) match {
      case Success(result, _) => scala.util.Success(result)
      case Failure(msg, left) => scala.util.Failure(new exceptions.ParserException("Expression could not be parsed: " + msg))
      case Error(msg, left) => scala.util.Failure(new exceptions.ParserException("ExpressionParser threw an exception: " + msg))
    }
  }
}