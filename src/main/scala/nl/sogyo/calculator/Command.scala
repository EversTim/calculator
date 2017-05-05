package nl.sogyo.calculator

import scala.util.{Try, Success, Failure}

trait Command

object Command {
  def apply(input: String): Try[Command] = CommandParser(input)
}

case class SimpleExpression(expr: Expression) extends Command

case class SetVariable(name: Variable, value: Expression) extends Command

case class PrintRPN(expr: Expression) extends Command

case object Exit extends Command
