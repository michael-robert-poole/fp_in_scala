package scala

object chapter2 extends App {
  // Exercise 2.1 - fib number starting 0,1,1,2,3,5
  def fib(n: Int): Int = {
    if (n == 0) 0 else if (n == 1) 1 else fib(n - 1) + fib(n - 2)

  }

  val l = List(0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10)
  l.foreach(x => println(fib(x)))

  // Exercise 2.2
  def isSorted[A](as: Array[A])(ordered: (A, A) => Boolean): Boolean = {
    if (as.isEmpty || as.tail.isEmpty) true else if (!ordered(as.head, as.tail.head)) false else isSorted(as.tail)(ordered)
  }

  val oListInt: Array[Int] = Array(1, 2, 3, 4)
  val uListInt: Array[Int] = Array(1, 3, 2, 4)
  println(isSorted(oListInt)((x, y) => x <= y))
  println(isSorted(uListInt)((x, y) => x <= y))

  //Exercise 2.3
  def curry[A, B, C](f: (A, B) => C): A => B => C = {
    a: A => b: B => f(a, b)
  }

  //Exercise 2.5
  //g(a) returns B
  //f(B) returns C
  def compose[A, B, C](f: B => C, g: A => B): A => C = a => f(g(a))

  val cInt: Int => Int => Int = curry((a, b) => a + b)
  val cString: String => String => String = curry((a, b) => a + b)
  val partialCInt: Int => Int = cInt(1000)
  val partialCString: String => String = cString("Scala ")
  println(cInt(1)(2))
  println(partialCInt(200))
  println(cString("hello ")("world"))
  println(partialCString("is great "))

  val comp: Int => Int = compose[Int, Int, Int](x => x * 100, y => y - 20)
  println(comp(10))
}
