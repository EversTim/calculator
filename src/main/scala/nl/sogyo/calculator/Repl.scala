package nl.sogyo.calculator

import scala.util.{ Try, Success, Failure }
import scala.io.StdIn.readLine
import scala.annotation.tailrec

object Repl {
  def main(args: Array[String]): Unit = {
    println(Parser("(2+3)(4+5)"))
    return
    if (args.length != 0) {
      val fullArgs = args.foldLeft("") { case (s, acc) => s + acc }
      println(fullArgs)
      tryPrint(Parser(fullArgs).value)
    } else repl
  }

  // SBT bug: doesn't show input
  @tailrec
  def repl(): Unit = {
    val read = readLine("$: ")
    if (read == "exit") return
    val eval = Parser(read)
    tryPrint(eval.value)
    repl()
  }

  def tryPrint(value: Try[Double]) {
    println(value match {
      case Success(s) => s
      case Failure(t) => t.getMessage
    })
  }
}