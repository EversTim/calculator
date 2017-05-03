/*package nl.sogyo.calculator.retired

import org.scalatest._

class ComplexSpec extends FlatSpec with Matchers {
  "A CartesianComplex" should "have string 1.0 + 2.0i for CartesianComplex(1, 2)" in {
    CartesianComplex(1, 2).toString should be("1.0 + 2.0i")
  }

  it should "have string 1.0 - 2.0i for CartesianComplex(1, -2.0)" in {
    CartesianComplex(1, -2).toString should be("1.0 - 2.0i")
  }
  
  it should "have string 1.0 for CartesianComplex(1, 0)" in {
    CartesianComplex(1, 0).toString should be("1.0")
  }

  "A Complex" should "be 3+5i when adding 1+2.5i and 2+2.5i" in {
    (CartesianComplex(1, 2.5) + CartesianComplex(2, 2.5)) should be(CartesianComplex(3, 5))
  }

  it should "be -1-5i when taking the negative of 1+5i" in {
    (-CartesianComplex(1, 5)) should be(CartesianComplex(-1, -5))
  }

  it should "be -1+0i when subtracting 1+2.5i and 2+2.5i" in {
    (CartesianComplex(1, 2.5) - CartesianComplex(2, 2.5)) should be(CartesianComplex(-1, 0))
  }

  it should "be cartesian 1 when polar is exp(0) as cartesian" in {
    PolarComplex(1, 0).asCartesian should be(CartesianComplex(1, 0))
  }

  it should "be cartesian 1 as polar when polar is exp(0)" in {
    PolarComplex(1, 0) should be(CartesianComplex(1, 0).asPolar)
  }

  it should "be cartesian 1 when polar is exp(0) (no casts)" in {
    PolarComplex(1, 0) should be(CartesianComplex(1, 0))
  }

  it should "be 2epi when 1epi is added to itself" in {
    (PolarComplex(1, math.Pi) + PolarComplex(1, math.Pi)) should be(PolarComplex(2, math.Pi))
  }
}*/