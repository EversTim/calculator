package nl.sogyo.calculator

import scala.util.parsing.combinator._

object Parser extends RegexParsers {
  def apply(input: String): Expression = parseAll(expr, input) match {
    case Success(result, _) => result
    case Failure(msg, _) => { println(msg); Empty }
    case Error(msg, _) => { println(msg); Empty }
  }

  def expr = plusminus

  def numberLike = number | literal
  
  def number: Parser[Expression] = """(-?[0-9]+(\.[0-9]+)?)""".r ^^ { x => Number(x.toDouble) }
  
  def literal: Parser[Expression] = """PI""".r ^^ { x => Number(math.Pi) }

  def plusminus: Parser[Expression] = multiplydivide ~ rep("+" ~ multiplydivide | "-" ~ multiplydivide) ^^ {
    case num ~ list => list.foldLeft(num) {
      case (x, "+" ~ y) => Plus(x, y)
      case (x, "-" ~ y) => Minus(x, y)
    }
  }

  def multiplydivide: Parser[Expression] = power ~ rep("*" ~ power | "/" ~ power) ^^ {
    case num ~ list => list.foldLeft(num) {
      case (x, "*" ~ y) => Times(x, y)
      case (x, "/" ~ y) => Divide(x, y)
    }
  }

  def power: Parser[Expression] = rep(parens ~ "^") ~ parens ^^ {
    case list ~ num => list.foldRight(num) {
      case (x ~ "^", y) => Power(x, y)
    }
  }

  def parens: Parser[Expression] = numberLike | ("(" ~> plusminus <~ ")")
}