package nl.sogyo.calculator.retired

trait Complex {
  def asCartesian: CartesianComplex
  def asPolar: PolarComplex

  def re: Double
  def im: Double

  def r: Double
  def theta: Double
  
  def isReal: Boolean
  def length: Double

  def +(that: Complex): Complex
  def -(that: Complex): Complex = this + (-that)
  def unary_-(): Complex

  def *(that: Complex): Complex
  def /(that: Complex): Complex
  
  def pow(that: Complex): Complex = if(this.isReal && that.isReal) Complex(math.pow(this.re, that.re)) else
    PolarComplex(math.pow(this.length, that.re/2), -that.im * this.theta)

  override def equals(that: Any): Boolean
}

object Complex {
  def apply(d: Double): Complex = CartesianComplex(d, 0)
}

case class CartesianComplex(val re: Double, val im: Double) extends Complex {
  
  def asCartesian = this
  def asPolar = PolarComplex(this.r, this.theta)

  def r = math.sqrt(this.length)
  def theta = math.atan2(this.im, this.re)
  
  def isReal: Boolean = this.im == 0
  def length: Double = this.re * this.re + this.im * this.im

  def +(that: Complex): Complex = CartesianComplex(this.re + that.re, this.im + that.im)
  def unary_- = CartesianComplex(-this.re, -this.im)

  def *(that: Complex): Complex = CartesianComplex(this.re * that.re - this.im * that.im, this.im * that.re + this.re * that.im)
  def /(that: Complex): Complex = if (that.r == 0) throw new ArithmeticException(this.toString)
  else CartesianComplex((this.re * that.re + this.im * that.im) / (that.re * that.re + that.im * that.im),
    (this.im * that.re - this.re * that.im) / (that.re * that.re + that.im * that.im))

  override def toString = re + (if (im != 0) { " " + (if (im < 0) "-" else "+") + " " + im.abs + "i" } else "")

  override def equals(that: Any): Boolean = that match {
    case cc: CartesianComplex => (this.re == cc.re) && (this.im == cc.im)
    case pol: PolarComplex => this.equals(pol.asCartesian)
    case d: Double => (this.re == d) && (this.im == 0)
    case _ => false
  }
}

case class PolarComplex(val r: Double, val theta: Double) extends Complex {
  def asPolar = this
  def asCartesian = CartesianComplex(re, im)

  def re = r * math.cos(theta)
  def im = r * math.sin(theta)
  
  def isReal: Boolean = this.theta == 0
  def length: Double = this.r * this.r

  def +(that: Complex): Complex = (this.asCartesian + that.asCartesian).asPolar
  def unary_-(): Complex = PolarComplex(-this.r, this.theta)

  def *(that: Complex): Complex = PolarComplex(this.r * that.r, this.theta + that.theta)
  def /(that: Complex): Complex = PolarComplex(this.r / that.r, this.theta - that.theta)

  override def toString = r + " exp(" + theta + "i)"

  override def equals(that: Any): Boolean = that match {
    case cc: CartesianComplex => this.equals(cc.asPolar)
    case pol: PolarComplex => (this.r == pol.r) && (this.theta == pol.theta)
    case d: Double => (this.r == d) && (this.theta == 0)
    case _ => false
  }
}