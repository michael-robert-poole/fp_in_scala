package scala


sealed trait StreamA[+A] {
  def headOption: Option[A]
  def headOption2: Option[A]

  def tail: StreamA[A]

  def toList[B >: A](ls: List[B]=List.empty): List[B]

  def take(n: Int): StreamA[A]

  def drop(n: Int): StreamA[A]

  def takeWhile(p : A => Boolean): StreamA[A]

  def takeWhile2(p : A => Boolean): StreamA[A]

  def forAll(p :A => Boolean):Boolean

  def foldRight[B](z:B)(f: (A,B) => B) : B

  def forEach(f: A => Unit): Unit
  def length: Int
}

case class ConsSA[+A](h: () => A, t: () => StreamA[A]) extends StreamA[A] {
  override def headOption: Option[A] = Some(h())
 override def headOption2: Option[A] = this.foldRight(Option.empty[A])((h, _) => Some(h))

  override def tail: StreamA[A] = t()

  override def toList[B >: A](ls: List[B]=List.empty): List[B] = h() :: t().toList(ls)

  override def take(n: Int): StreamA[A] = if (n == 0) StreamA.empty else StreamA.consSA(h(), tail.take(n - 1))

  override def drop(n: Int): StreamA[A] = if (n == 0) StreamA.consSA(h(), tail.drop(n)) else tail.drop(n - 1)

  override def takeWhile(p: A => Boolean): StreamA[A] = if (p(h())) StreamA.consSA(h(), t().takeWhile(p)) else StreamA.empty
  override def takeWhile2(p: A => Boolean): StreamA[A] = this.foldRight(StreamA.empty[A])((h,t) => if(p(h)) StreamA.consSA(h, t) else StreamA.empty)

  override def forAll(p: A => Boolean): Boolean = p(h()) && t().forAll(p); println(t())

  override def foldRight[B](z: B)(f: (A, B) => B): B = f(h(), t().foldRight(z)(f))

  override def forEach(f: A => Unit): Unit = this.foldRight(())((x,_) => f(x))

  override def length: Int = this.foldRight(0)((h, l)=> l+1)
}


case object Empty extends StreamA[Nothing] {
  override def headOption: Option[Nothing] = None
  override def headOption2: Option[Nothing] = None

  override def tail: StreamA[Nothing] = Empty

  override def toList[B >: Nothing](ls: List[B]): List[B] = ls.reverse

  override def take(n: Int): StreamA[Nothing] = Empty

  override def drop(n: Int): StreamA[Nothing] = Empty

  override def takeWhile(p: Nothing => Boolean): StreamA[Nothing] = Empty
  override def takeWhile2(p: Nothing => Boolean): StreamA[Nothing] = Empty

  override def forAll(p: Nothing => Boolean): Boolean = true

  override def foldRight[B](z: B)(f: (Nothing, B) => B): B = z

  override def forEach(f: Nothing => Unit): Unit = println("End")

  override def length: Int = 0
}

object StreamA {
  def consSA[A](hd: => A, tail: => StreamA[A]): StreamA[A] = {
    lazy val h = hd
    lazy val t = tail
    ConsSA( () => h, ()  => t)
  }

  def empty[A]: StreamA[A] = Empty

  def apply[A](as: A*): StreamA[A] = {
    if (as.isEmpty) empty else consSA(as.head, apply(as.tail: _*))
  }
}
