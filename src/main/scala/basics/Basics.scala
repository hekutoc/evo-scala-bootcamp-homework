package main.scala.basics

object Basics {
  def gcd(a: Int, b: Int, divider: Int = 2): Int = {
    val lo = scala.math.min(a, b)
    val hi = scala.math.max(a, b)
    if (divider <= lo) {
      if (lo % divider == 0 && hi % divider == 0) {
        divider * gcd(lo / divider, hi / divider, divider)
      } else {
        gcd(lo, hi, divider + 1)
      }
    } else {
      1
    }
  }

  def lcm(a: Int, b:Int): Int = {
    a * b / gcd(a, b)
  }

  def main(args: Array[String]): Unit = {
    println(gcd(10,11))
  }
}
