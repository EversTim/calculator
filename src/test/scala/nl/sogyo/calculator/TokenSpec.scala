package nl.sogyo.calculator

import org.scalatest._
import scala.util.{ Try, Success, Failure }

class TokenSpec extends FlatSpec with Matchers {
  "A Token" should "be Number(5) when 5 is fed" in {
    val tkn = Token("5")
    tkn should be (Success(Number(5)))
  }
  
  it should "be Plus when + is fed" in {
    val tkn = Token("+")
    tkn should be (Success(Plus))
  }
  
  it should "be Number(-40) when -40 is fed" in {
    val tkn = Token("-40")
    tkn should be (Success(Number(-40)))
  }
}