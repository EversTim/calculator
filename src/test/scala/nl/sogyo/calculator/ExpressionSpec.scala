package nl.sogyo.calculator

import org.scalatest._
import scala.util.{ Try, Success, Failure }

class ExpressionSpec extends FlatSpec with Matchers {
  "An Expression" should "return the value.get 3 when it equals Number(3)" in {
    Number(3).value().get should be(3)
  }

  it should "return the value.get 5.0 when it equals Plus(Number(2), Number(3))" in {
    Plus(Number(2), Number(3)).value().get should be(5)
  }

  it should "return the value.get 14.0 on Parser(\"1*2+3* 4\")" in {
    ExpressionParser("1*2+3* 4").value.get should be(14)
  }

  it should "return the value.get 11.0 on Parser(\"1+2*3+4\")" in {
    ExpressionParser("1+2*3+4").value.get should be(11)
  }

  it should "return the value.get 0.0 on Parser(\"1+4/2-3\")" in {
    ExpressionParser("1+4/2-3").value.get should be(0)
  }

  it should "return the value.get 2.0 on Parser(\"2^1^2\")" in {
    ExpressionParser("2^1^2").value.get should be(2)
  }

  it should "return the value.get 2.0 on Parser(\"2^(1^2)\")" in {
    ExpressionParser("2^(1^2)").value.get should be(2)
  }

  it should "return the value.get 4.0 on Parser(\"(2^1)^2\")" in {
    ExpressionParser("(2^1)^2").value.get should be(4)
  }

  it should "return the value 2Pi on Parser(\"2*PI\")" in {
    ExpressionParser("2*PI").value.get should be(2 * math.Pi)
  }

  it should "return RPN 3 4 5 * - on 3 - 4 * 5" in {
    ExpressionParser("3 - 4 * 5").toString should be("3.0 4.0 5.0 * -")
  }

  it should "return RPN 3 4 - 5 * on (3 - 4) * 5" in {
    ExpressionParser("(3 - 4) * 5").toString should be("3.0 4.0 - 5.0 *")
  }

  it should "return RPN 3 4 - 5 * on (3 - 4) * 5 with Expression.apply" in {
    Expression("(3 - 4) * 5").toString should be("3.0 4.0 - 5.0 *")
  }

  it should "return 5 on \"x\" with x=5" in {
    Expression("x").value(Map("x" -> Number(5))).get should be(5)
  }

  it should "return 15 on \"2x +5\" with x=5" in {
    Expression("2x + 5").value(Map("x" -> Number(5))).get should be(15)
  }

  it should "return 15 on \"2x +5\" with x=2+3" in {
    Expression("2x + 5").value(Map("x" -> Expression("2+3"))).get should be(15)
  }
}