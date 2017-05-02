package nl.sogyo.calculator

trait Complex {
  def asCartesian: CartesianComplex
  def asPolar: PolarComplex

  def re: Double
  def im: Double

  def r: Double
  def theta: Double

  def +(that: Complex): Complex
  def -(that: Complex): Complex = this + (-that)
  def unary_-(): Complex

  override def equals(that: Any): Boolean
}

case class CartesianComplex(val re: Double, val im: Double) extends Complex {
  def asCartesian = this
  def asPolar = PolarComplex(r, theta)

  def r = math.sqrt(re * re + im * im)
  def theta = math.atan2(im, re)

  def +(that: Complex): Complex = CartesianComplex(this.re + that.re, this.im + that.im)
  def unary_- = CartesianComplex(-this.re, -this.im)

  override def toString = re + (if (im != 0) { " " + (if (im < 0) "-" else "+") + " " + im.abs + "i" } else "")

  override def equals(that: Any): Boolean = that match {
    case cc: CartesianComplex => (this.re == cc.re) && (this.im == cc.im)
    case pol: PolarComplex => this.equals(pol.asCartesian)
    case _ => false
  }
}

case class PolarComplex(val r: Double, val theta: Double) extends Complex {
  def asPolar = this
  def asCartesian = CartesianComplex(re, im)

  def re = r * math.cos(theta)
  def im = r * math.sin(theta)

  def +(that: Complex): Complex = (this.asCartesian + that.asCartesian).asPolar

  def unary_-(): Complex = PolarComplex(-this.r, this.theta)

  override def toString = r + " exp(" + theta + "i)"

  override def equals(that: Any): Boolean = that match {
    case cc: CartesianComplex => this.equals(cc.asPolar)
    case pol: PolarComplex => (this.r == pol.r) && (this.theta == pol.theta)
    case _ => false
  }
}