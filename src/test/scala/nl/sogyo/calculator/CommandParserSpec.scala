package nl.sogyo.calculator

import org.scalatest._
import scala.util.{ Try, Success, Failure }

class CommandParserSpec extends FlatSpec with Matchers {
  "A CommandParser" should "return Success(SimpleExpression(Plus(Number(2), Number(3)))) when given \"2+3\"" in {
    CommandParser("2+3") should be(Success(SimpleExpression(Plus(Number(2), Number(3)))))
  }
  
  it should "return Success(Exit) when given \"exit\"" in {
    CommandParser("exit") should be(Success(Exit))
  }
  
  it should "return Success(SetVariable(Variable(\"x\"), Number(5))) when given \"x=5\"" in {
    CommandParser("x=5") should be(Success(SetVariable(Variable("x"), Number(5))))
  }
}