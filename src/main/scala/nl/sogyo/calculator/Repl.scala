package nl.sogyo.calculator

import scala.util.{ Try, Success, Failure }
import scala.io.StdIn.readLine
import scala.annotation.tailrec
import nl.sogyo.calculator.logic._

object Repl {

  var variables: Map[Variable, Expression] = Map()

  def main(args: Array[String]): Unit = {
    if (args.length != 0) {
      val fullArgs = args.foldLeft("") { case (s, acc) => s + acc }
      evaluate(fullArgs)
    } else repl()
  }

  def evaluate(read: String): Unit = {
    val eval = Command(read)
    checkCommand(eval)
  }

  // SBT bug: doesn't show input
  @tailrec
  def repl(): Unit = {
    val read = readLine("^_^ ")
    evaluate(read)
    repl()
  }

  def checkCommand(tCmd: Try[Command]): Unit = tCmd match {
    case Success(cmd) => execute(cmd)
    case Failure(t) => println(t.getMessage)
  }

  def execute(cmd: Command): Unit = cmd match {
    case Exit => System.exit(0)
    case SimpleExpression(expr) => tryPrintValue(expr.value(variables))
    case SetVariable(v, expr) => variables = variables + (v -> expr)
    case PrintRPN(expr) => println(expr.toString)
  }

  def tryPrintValue(input: Try[Double]): Unit = println(input match {
    case Success(v) => v
    case Failure(t) => t.getMessage
  })
}
