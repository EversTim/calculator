package nl.sogyo.calculator.logic

import scala.util.parsing.combinator._
import scala.util.{Try, Success, Failure}

trait CommandParser extends RegexParsers with ExpressionParser {

  def cmd: Parser[Command] = keyword | setVariable | simpleExpression

  def keyword: Parser[Command] = exit | print

  def exit: Parser[Command] = "exit" ^^ { x => Exit }

  def print: Parser[PrintRPN] = "print(" ~> expr <~ ")" ^^ PrintRPN

  def simpleExpression: Parser[SimpleExpression] = expr ^^ SimpleExpression

  def setVariable: Parser[SetVariable] = (variable ~ "=" ~ expr) ^^ { case v ~ "=" ~ e => SetVariable(v, e) }
}

object CommandParser extends CommandParser {
  def apply(input: String): Try[Command] = parseAll(cmd, input) match {
    case Success(result, _) => scala.util.Success(result)
    case Failure(msg, left) => scala.util.Failure(new exceptions.ParserException("Command could not be parsed: " + msg))
    case Error(msg, left) => scala.util.Failure(new exceptions.ParserException("CommandParser threw an exception: " + msg))
  }
}