package nl.sogyo.calculator

import scala.util.parsing.combinator._

object Parser extends RegexParsers {
  def literals: Map[String, Double] = Map("PI" -> math.Pi, "E" -> math.E)
  def functions: Map[String, Double => Double] = Map("" -> { x => x }, "sin" -> math.sin, "cos" -> math.cos, "tan" -> math.tan, "exp" -> math.exp)

  def apply(input: String): Expression = parseAll(expr, input) match {
    case Success(result, _) => result
    case Failure(msg, left) => { println(msg + " " + left); Empty }
    case Error(msg, left) => { println(msg + " " + left); Empty }
  }

  def expr = plusMinus

  def number: Parser[Expression] = """(-?[0-9]+(\.[0-9]+)?)""".r ^^ { x => Number(x.toDouble) }

  def text = function | literal | parens

  def literal: Parser[Expression] = number | """[A-Z]+""".r ^^ { x => Number(literals(x)) }

  def plusMinus: Parser[Expression] = multiplyDivide ~ rep("+" ~ multiplyDivide | "-" ~ multiplyDivide) ^^ {
    case num ~ list => list.foldLeft(num) {
      case (x, "+" ~ y) => Plus(x, y)
      case (x, "-" ~ y) => Minus(x, y)
    }
  }

  def multiplyDivide: Parser[Expression] = explicitMultiplication// | implicitMultiplication

  def explicitMultiplication: Parser[Expression] = power ~ rep("*" ~ power | "/" ~ power) ^^ {
    case num ~ list => list.foldLeft(num) {
      case (x, "*" ~ y) => Times(x, y)
      case (x, "/" ~ y) => Divide(x, y)
    }
  }

  def implicitMultiplication: Parser[Expression] = ((literal | text | power) ~ rep(text)) ^^ {
    case num ~ list => list.foldLeft(num) {
      case (x, y) => Times(x, y)
    }
  }

  def power: Parser[Expression] = rep(parens ~ "^") ~ parens ^^ {
    case list ~ num => list.foldRight(num) {
      case (x ~ "^", y) => Power(x, y)
    }
  }

  def parens: Parser[Expression] = literal | function | ("(" ~> plusMinus <~ ")")

  def function: Parser[Expression] = ("""[a-z]+\(""".r ~ plusMinus <~ ")") ^^ { case str ~ expr => Function(functions(str.init), str.init, expr) }
}