package nl.sogyo.calculator

import scala.util.parsing.combinator._

import scala.util.Try

trait CommandParser extends RegexParsers with ExpressionParser {

  def cmd: Parser[Command] = exit | simpleExpression | setVariable

  def exit = "exit" ^^ { x => Exit }

  def simpleExpression: Parser[SimpleExpression] = expr ^^ (x => SimpleExpression(x))

  def setVariable: Parser[SetVariable] = (variable ~ "=" ~ expr) ^^ { case v ~ "=" ~ e => SetVariable(v, e) }
}

object CommandParser extends CommandParser {
  def apply(input: String): Try[Command] = parseAll(cmd, input) match {
    case Success(result, _) => scala.util.Success(result)
    case Failure(msg, left) => scala.util.Failure(new exceptions.ParserException("Command could not be parsed: " + msg))
    case Error(msg, left) => scala.util.Failure(new exceptions.ParserException("CommandParser threw an exception: " + msg))
  }
}