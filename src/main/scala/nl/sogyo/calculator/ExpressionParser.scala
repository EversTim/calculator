package nl.sogyo.calculator

import scala.util.parsing.combinator._

object ExpressionParser extends RegexParsers {
  def literals: Map[String, Double] = Map("PI" -> math.Pi, "E" -> math.E)
  def functions: Map[String, Double => Double] = Map("sin" -> math.sin, "cos" -> math.cos, "tan" -> math.tan, "exp" -> math.exp)

  def apply(input: String): Expression = {
    parseAll(expr, input) match {
      case Success(result, _) => result
      case Failure(msg, left) => Empty
      case Error(msg, left) => Empty
    }
  }

  def expr = plusMinus

  def number: Parser[Expression] = """(-?[0-9]+(\.[0-9]+)?)""".r ^^ { x => Number(x.toDouble) }

  def literal: Parser[Expression] = """[A-Z]+""".r ^^ { x => Number(literals(x)) }

  def parens: Parser[Expression] = ("(" ~> plusMinus <~ ")")

  def function: Parser[Expression] = ("""[a-z]{2,}\(""".r ~ plusMinus <~ ")") ^^ { case str ~ expr => Function(functions(str.init), str.init, expr) }

  def variable: Parser[Expression] = """[a-z]""".r ^^ (Variable(_))

  def text = function | literal | parens | variable

  def numberLike = literal | number | variable

  def plusMinus: Parser[Expression] = multiplyDivide ~ rep("+" ~ multiplyDivide | "-" ~ multiplyDivide) ^^ {
    case num ~ list => list.foldLeft(num) {
      case (x, "+" ~ y) => Plus(x, y)
      case (x, "-" ~ y) => Minus(x, y)
    }
  }

  def multiplyDivide: Parser[Expression] = implicitMultiplication | explicitMultiplicationDivision

  def explicitMultiplicationDivision: Parser[Expression] = power ~ rep("*" ~ power | "/" ~ power) ^^ {
    case num ~ list => list.foldLeft(num) {
      case (x, "*" ~ y) => Times(x, y)
      case (x, "/" ~ y) => Divide(x, y)
    }
  }

  def implicitMultiplication: Parser[Expression] = {
    def exprList = (text | number) ~ rep1(text) ^^ { case exprA ~ list => list.foldLeft(exprA) { case (x, y) => Times(x, y) } }
    def listExpr = rep1(text) ~ number ^^ { case list ~ exprA => list.foldRight(exprA) { case (x, y) => Times(x, y) } }
    def listExprList = rep1(text) ~ number ~ rep1(text) ^^ {
      case listA ~ num ~ listB => {
        val fold1 = listA.foldRight(num) {
          case (x, y) => Times(x, y)
        }
        listB.foldLeft(fold1) {
          case (x, y) => Times(x, y)
        }
      }
    }

    listExprList | exprList | listExpr
  }

  def power: Parser[Expression] = rep(recurse <~ "^") ~ recurse ^^ {
    case list ~ num => list.foldRight(num) {
      case (x, y) => Power(x, y)
    }
  }

  def recurse = text | number
}