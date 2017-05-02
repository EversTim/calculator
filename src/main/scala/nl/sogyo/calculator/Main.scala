package nl.sogyo.calculator

object Main {
  def main(args: Array[String]): Unit = {
    //println(Parser(args(0)).value.get)
    
    val cart = CartesianComplex(1, 0)
    val pol = PolarComplex(1, 0)
    
    println(cart == cart)
    println(pol == pol)
    println(cart == pol)
  }
}