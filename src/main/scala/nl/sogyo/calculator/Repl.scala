package nl.sogyo.calculator

import scala.util.{ Try, Success, Failure }
import scala.io.StdIn.readLine
import scala.annotation.tailrec

object Repl {
  def main(args: Array[String]): Unit = {
    if (args.length != 0) {
      val fullArgs = args.foldLeft("") { case (s, acc) => s + acc }
      val eval = Expression(fullArgs)
      tryPrint(eval.value)
    } else repl
  }

  // SBT bug: doesn't show input
  @tailrec
  def repl(): Unit = {
    val read = readLine("$: ")
    if (read == "exit") return
    val eval = Expression(read)
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