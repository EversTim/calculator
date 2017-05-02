package nl.sogyo.calculator

import org.scalatest._
import scala.util.{ Try, Success, Failure }

class ParserSpec extends FlatSpec with Matchers {
  "Parser" should "return 3 4 + on 3 + 4" in {
    val output = Parser.parse("3 + 4")
    output should be(Success(List(Number(3), Number(4), Plus)))
  }

  it should "fail on bladibla" in {
    val output = Parser.parse("bladibla")
    output.isFailure should be(true)
  }

  it should "return 3 4 - 5 + on 3 - 4 + 5" in {
    val output = Parser.parse("3 - 4 + 5")
    output should be(Success(List(Number(3), Number(4), Minus, Number(5), Plus)))
  }

  it should "return 3 4 2 1 - * + on 3 + 4 * ( 2 - 1 )" in {
    val output = Parser.parse("3 + 4 * ( 2 - 1 )")
    output should be(Success(List(Number(3), Number(4), Number(2), Number(1), Minus, Times, Plus)))
  }

  it should "return 3 4 2 * 1 5 - 2 3 ^ ^ / + on 3 + 4 * 2 / ( 1 - 5 ) ^ 2 ^ 3" in {
    val output = Parser.parse("3 + 4 * 2 / ( 1 - 5 ) ^ 2 ^ 3")
    output should be(Success(List(Number(3), Number(4), Number(2), Times, Number(1), Number(5), Minus, Number(2), Number(3), Power, Power, Divide, Plus)))
  }
}