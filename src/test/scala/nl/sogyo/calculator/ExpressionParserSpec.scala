package nl.sogyo.calculator

import org.scalatest._

class ExpressionParserSpec extends FlatSpec with Matchers {
  "An ExpressionParser" should "return Number(3) when given \"3\"" in {
    val output = ExpressionParser("3")
    output.get should be(Number(3))
  }

  it should "return Number(40) when given \"40 \"" in {
    val output = ExpressionParser("40 ")
    output.get should be(Number(40))
  }

  it should "return Number(-100) when given \" -100\"" in {
    val output = ExpressionParser(" -100")
    output.get should be(Number(-100))
  }

  it should "return Number(-100.1) when given \" -100.1\"" in {
    val output = ExpressionParser(" -100.1")
    output.get should be(Number(-100.1))
  }

  it should "return Plus(Number(-100), Number(3)) when given \"-100 + 3\"" in {
    val output = ExpressionParser("-100 + 3")
    output.get should be(Plus(Number(-100), Number(3)))
  }

  it should "return Plus(Number(-100), Number(3)) when given \"-100+3\"" in {
    val output = ExpressionParser("-100+3")
    output.get should be(Plus(Number(-100), Number(3)))
  }

  it should "return Plus(Number(3), Number(-100)) when given \"3+-100\"" in {
    val output = ExpressionParser("3+-100")
    output.get should be(Plus(Number(3), Number(-100)))
  }

  it should "return Plus(Plus(Number(1), Number(2)), Number(3)) when given \"1+2+3\"" in {
    val output = ExpressionParser("1+2+3")
    output.get should be(Plus(Plus(Number(1), Number(2)), Number(3)))
  }

  it should "return Plus(Minus(Number(1), Number(2)), Number(3)) when given \"1-2+3\"" in {
    val output = ExpressionParser("1-2+3")
    output.get should be(Plus(Minus(Number(1), Number(2)), Number(3)))
  }

  it should "return Minus(Number(1), Number(-2)) when given \"1--2\"" in {
    val output = ExpressionParser("1--2")
    output.get should be(Minus(Number(1), Number(-2)))
  }

  it should "return Times(Number(1), Number(-2)) when given \"1*-2\"" in {
    val output = ExpressionParser("1*-2")
    output.get should be(Times(Number(1), Number(-2)))
  }

  it should "return Divide(Number(1), Number(-2)) when given \"1/-2\"" in {
    val output = ExpressionParser("1/-2")
    output.get should be(Divide(Number(1), Number(-2)))
  }

  it should "return Plus(Times(Number(1), Number(2)), Times(Number(3), Number(4))) when given \"1*2+3* 4\"" in {
    val output = ExpressionParser("1*2+3* 4")
    output.get should be(Plus(Times(Number(1), Number(2)), Times(Number(3), Number(4))))
  }

  it should "return Plus(Plus(Number(1), Times(Number(2), Number(3))), Number(4)) when given \"1+2*3+4\"" in {
    val output = ExpressionParser("1+2*3+4")
    output.get should be(Plus(Plus(Number(1), Times(Number(2), Number(3))), Number(4)))
  }

  it should "return Times(Times(Number(1), Plus(Number(2), Number(3))), Number(4)) when given \"1*(2+3)* 4\"" in {
    val output = ExpressionParser("1*(2+3)* 4")
    output.get should be(Times(Times(Number(1), Plus(Number(2), Number(3))), Number(4)))
  }

  it should "return Power(Number(2), Power(Number(3), Number(4))) when given \"2 ^ 3 ^ 4\"" in {
    val output = ExpressionParser("2^3^4")
    output.get should be(Power(Number(2), Power(Number(3), Number(4))))
  }

  it should "return Power(Number(2), Plus(Number(1), Number(2))) when given \"2^(1+2)\"" in {
    val output = ExpressionParser("2^(1+2)")
    output.get should be(Power(Number(2), Plus(Number(1), Number(2))))
  }

  it should "return Number(pi) when given PI" in {
    val output = ExpressionParser("PI")
    output.get should be(Number(math.Pi))
  }

  it should "return Number(e) when given E" in {
    val output = ExpressionParser("E")
    output.get should be(Number(math.E))
  }

  it should "return Function(sin, \"sin\", Number(1)) when given sin(1)" in {
    val output = ExpressionParser("sin(1)")
    output.get should be(Function(math.sin, "sin", Number(1)))
  }

  it should "return Function(exp, \"exp\", Number(1)) when given exp(1)" in {
    val output = ExpressionParser("exp(1)")
    output.get should be(Function(math.exp, "exp", Number(1)))
  }

  it should "return Times(Number(2), Function(sin, \"sin\", Number(1)) when given \"2sin(1)\"" in {
    val output = ExpressionParser("2sin(1)")
    output.get should be(Times(Number(2), Function(math.sin, "sin", Number(1))))
  }

  it should "return Times(Number(2), Number(math.Pi)) when given \"2PI\"" in {
    val output = ExpressionParser("2PI")
    output.get should be(Times(Number(2), Number(math.Pi)))
  }

  it should "return Times(Plus(Number(2), Number(3)), Plus(Number(4), Number(5))) when given \"(2+3)(4+5)\"" in {
    val output = ExpressionParser("(2+3)(4+5)")
    output.get should be(Times(Plus(Number(2), Number(3)), Plus(Number(4), Number(5))))
  }

  it should "return Times(Times(Plus(Number(2), Number(3)), Plus(Number(4), Number(5))), Plus(Number(6), Number(7))) when given \"(2+3)(4+5)(6+7)\"" in {
    val output = ExpressionParser("(2+3)(4+5)(6+7)")
    output.get should be(Times(Times(Plus(Number(2), Number(3)), Plus(Number(4), Number(5))), Plus(Number(6), Number(7))))
  }

  it should "return Times(Plus(Number(2), Number(3)), Function(sin, \"sin\", Number(1)) when given \"(2+3)sin(1)\"" in {
    val output = ExpressionParser("(2+3)sin(1)")
    output.get should be(Times(Plus(Number(2), Number(3)), Function(math.sin, "sin", Number(1))))
  }

  it should "return Times(Function(sin, \"sin\", Number(1), Plus(Number(2), Number(3))) when given \"sin(1)(2+3)\"" in {
    val output = ExpressionParser("sin(1)(2+3)")
    output.get should be(Times(Function(math.sin, "sin", Number(1)), Plus(Number(2), Number(3))))
  }

  it should "return Times(Number(1), Plus(Number(2), Number(3))) when given \"1(2+3)\"" in {
    val output = ExpressionParser("1(2+3)")
    output.get should be(Times(Number(1), Plus(Number(2), Number(3))))
  }

  it should "return Times(Times(Number(2), Number(3)), Number(4)) when given \"(2)(3)(4)\"" in {
    val output = ExpressionParser("(2)(3)(4)")
    output.get should be(Times(Times(Number(2), Number(3)), Number(4)))
  }

  it should "return Variable(\"x\") when given \"x\"" in {
    val output = ExpressionParser("x")
    output.get should be(Variable("x"))
  }

  it should "return Times(Number(2), Variable(\"x\")) when given \"2x\"" in {
    val output = ExpressionParser("2x")
    output.get should be(Times(Number(2), Variable("x")))
  }

  it should "return Times(Divide(Number(1), Number(2)), Number(math.Pi)) when given \"1/2PI\"" in {
    val output = Expression("1/2PI")
    output.get should be(Times(Divide(Number(1), Number(2)), Number(math.Pi)))
  }
  it should "return Times(Times(Number(1), Number(2)), Number(math.Pi)) when given \"1*2PI\"" in {
    val output = Expression("1*2PI")
    output.get should be(Times(Times(Number(1), Number(2)), Number(math.Pi)))
  }

  it should "return Variable(\"bla\") when given \"bla\"" in {
    val output = Expression("bla")
    output.get should be(Variable("bla"))
  }
  
  it should "return Number(0.5) when given \".5\"" in {
    val output = Expression(".5")
    output.get should be(Number(0.5))
  }
  
  /* These tests are saved for historical reasons, and because they should be implemented in the future.
   * At the moment, they are not supported.
   * They concern implicit multiplication with numbers on the left.
   * Best implementation so far broke a-b to give a*(-b)
   */
  /*
  it should "give the same result on (2+3)4 as on (2+3) * 4" in {
    Expression("(2+3)4") should be(Expression("(2+3)*4"))
  }
  
  it should "return Times(Plus(Number(2), Number(3)), Number(1)) when given \"(2+3)1\"" in {
    val output = ExpressionParser("(2+3)1")
    output.get should be(Times(Plus(Number(2), Number(3)), Number(1)))
  }
  
  it should "return Times(Times(Number(2), Number(3)), Number(4)) when given \"(2)3(4)\"" in {
    val output = ExpressionParser("(2)3(4)")
    output.get should be(Times(Times(Number(2), Number(3)), Number(4)))
  }
  
  it should "return Times(Times(Plus(Number(3), Number(4)), Number(1)), Plus(Number(2), Number(3))) when given \"(3+4)1(2+3)\"" in {
    val output = ExpressionParser("(3+4)1(2+3)")
    output.get should be(Times(Times(Plus(Number(3), Number(4)), Number(1)), Plus(Number(2), Number(3))))
  }
  
  it should "return Times(Times(Number(2), Number(3)), Number(4)) when given \"(2)3*4\"" in {
    val output = Expression("(2)3*4")
    output.get should be(Times(Times(Number(2), Number(3)), Number(4)))
  }
  */
}