package scala

import scala.math.abs

sealed trait RNG{
  def nextInt(): (Int, RNG)
  def naturalNumber():Int
}
case class SimpleRNG(seed:Long) extends RNG{
  override def nextInt(): (Int, RNG) = {
    val newSeed = (seed * 0x5DEECE66DL + 0xBL) & 0xFFFFFFFFFFFFL
    val nexRNG = SimpleRNG(newSeed)
    val n = (newSeed >>> 16).toInt
    (n, nexRNG)
  }

  override def naturalNumber(): Int ={
    (abs(this.nextInt._1))
  }
}

object chapter6 extends App{
  val rng = SimpleRNG(42)
  val (n1, r1) = rng.nextInt
  println(n1)
  val (n2, r2) = r1.nextInt
  println(n2)
  val (n3, r3) = r2.nextInt
  println(n3)
  println(r1.naturalNumber())


}