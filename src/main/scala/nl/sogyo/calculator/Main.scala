package nl.sogyo.calculator

import scala.util.{Try, Success, Failure}

object Main {
  def main(args: Array[String]): Unit = {
    val parsed = Parser.parse("3 + 4 * 2 / ( 1 - 5 ) ^ 2 ^ 3")
    println(parsed)
  }
}