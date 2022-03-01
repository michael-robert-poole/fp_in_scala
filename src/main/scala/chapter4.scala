package scala
sealed trait OptionA[+A]{
  def map[B](f:A => B): OptionA[B]
  def flatMap[B](f:A => OptionA[B]): OptionA[B]
  def getOrElse[B >: A](default: => B): B
  def orElse[B >: A](ob: => OptionA[B]):OptionA[B]
  def filter(f: A => Boolean): OptionA[A]
}
case class SomeA[+A](get:A) extends OptionA[A] {
  override def map[B](f: A => B): OptionA[B] = SomeA(f(get))

  override def flatMap[B](f: A => OptionA[B]): OptionA[B] = map(f).getOrElse(NoneA)

  override def getOrElse[B >: A](default: => B): B = get


  override def filter(f:A => Boolean): OptionA[A] = if(f(get)) SomeA(get) else NoneA

  override def orElse[B >: A](ob: => OptionA[B]): OptionA[B] = ob
}
case object NoneA extends OptionA[Nothing] {
  override def map[B](f: Nothing => B): OptionA[B] = NoneA

  override def flatMap[B](f: Nothing => OptionA[B]): OptionA[B] = NoneA

  override def getOrElse[B >: Nothing](default: => B): B = default

  override def filter(f: Nothing => Boolean): OptionA[Nothing] = NoneA

  override def orElse[B >: Nothing](ob: => OptionA[B]): OptionA[B] = NoneA
}

object OptionA{
  def someA[A](get:A):OptionA[A] = SomeA(get)
  def noneA[A]:OptionA[A] = NoneA
}

object test extends App{
  val l = (List(1.0,2.0,3.0,10.0))


}
