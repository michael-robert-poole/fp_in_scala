package scala
import scala.math.Ordering.Implicits.infixOrderingOps

import ListA._

sealed trait ListA [+A]
case object Nils extends ListA[Nothing]
case class Cons[+A](head:A, tail:ListA[A]) extends ListA[A]

object ListA {
  def apply[A](as: A*): ListA[A] = if (as.isEmpty) Nils else Cons(as.head, apply(as.tail: _*))

  def headOption[A](ls: ListA[A]): Option[A] = ls match {
    case Nils => None
    case Cons(h, _) => Some(h)
  }

  def tail[A](ls: ListA[A]): ListA[A] = ls match {
    case Nils => Nils
    case Cons(_, t) => t
  }

  def setHead[A](ls: ListA[A], elem: A): ListA[A] = ls match {
    case Nils => Cons(elem, Nils)
    case Cons(_, t) => Cons(elem, t)
  }

  def drop[A](ls: ListA[A], n: Int): ListA[A] = ls match {
    case Nils => Nils
    case Cons(_, _) => if (n == 0) ls else drop(tail(ls), n - 1)
  }

  def dropWhile[A](ls: ListA[A], f: A => Boolean): ListA[A] = ls match {
    case Nils => Nils
    case Cons(h, t) => if (f(h)) dropWhile(t, f) else ls
  }

  def dropWhileCurry[A](ls: ListA[A])(f: A => Boolean): ListA[A] = ls match {
    case Nils => Nils
    case Cons(h, t) => if (f(h)) dropWhileCurry(t)(f) else ls
  }

  def init[A](ls: ListA[A]): ListA[A] = ls match {
    case Nils => Nils
    case Cons(_, Nils) => Nils
    case Cons(h, t) => Cons(h, init(t))
  }

  def foldRight[A, B](ls: ListA[A], z: B)(f: (A, B) => B): B = ls match {
    case Nils => z
    case Cons(h, t) => f(h, foldRight(t, z)(f))
  }

  //Exercise 3.8 and 3.9 Compute length and product using fold right
  def lengthR[A](ls: ListA[A]): Int = foldRight(ls, 0)((_, z) => z + 1)

  def productR(ls: ListA[Double]): Double = foldRight(ls, 1.0)(_ * _)

  //Exerise 3.10 - Write a fold left function

  def foldLeft[A, B](ls: ListA[A], z: B)(f: (B, A) => B): B = ls match {
    case Nils => z
    case Cons(h, t) => foldLeft(t, f(z, h))(f)
  }

  //Exercise 3.11 - write sum product length using fold Left

  def sum(ls: ListA[Double]): Double = foldLeft(ls, 0.0)(_ + _)

  def product(ls: ListA[Double]): Double = foldLeft(ls, 1.0)(_ * _)

  def length[A](ls: ListA[A]): Int = foldLeft(ls, 0)((z, _) => z + 1)

  //Exercise 3.12 - write a function to reverse a list
  //
  //Cons(2,Cons(1, Nil))
  //Cons(3,Cons(2, Cons(1, Nil)))
  //Cons(4,Cons(3, Cons(2, Cons(1,Nil))))
  def reverse[A](ls: ListA[A]): ListA[A] = foldLeft(ls, Nils: ListA[A])((t, h) => Cons(h, t))

  //Exercise 3.13
  def foldRightViaFoldLeft[A, B](ls: ListA[A], z: B)(f: (B, A) => B): B = foldLeft(reverse(ls), z)(f)

  //Exercise 3.14
  def append[A](ls: ListA[A], elem: A): ListA[A] = foldRight(ls, Cons(elem, Nils))((h, z) => Cons(h, z))

  //3.15
  def flatten[A](ls: ListA[ListA[A]]): ListA[A] = ls match {
    case Nils => Nils
    case Cons(l1, l2) => l1 match {
      case Nils => Nils
      case Cons(h, t) => Cons(h, foldRightViaFoldLeft(t, flatten(l2))((h, t) => Cons(t, h)))
    }
  }

  //Exercise 3.16
  def increment(ls: ListA[Int]): ListA[Int] = foldRightViaFoldLeft(ls, Nils: ListA[Int])((z, h) => Cons(h + 1, z))

  //Ex 3.17
  def doubleToString(ls: ListA[Double]): ListA[String] = foldRightViaFoldLeft(ls, Nils: ListA[String])((z, h) => Cons(h.toString, z))


  def map[A, B](ls: ListA[A])(f: A => B): ListA[B] = foldRightViaFoldLeft(ls, Nils: ListA[B])((z, h) => Cons(f(h), z))

  def map_2[A, B](ls: ListA[A])(f: A => B): ListA[B] = ls match {
    case Nils => Nils
    case Cons(h, t) => Cons(f(h), map(t)(f))
  }

  def filter[A](ls: ListA[A])(f: A => Boolean): ListA[A] = foldRightViaFoldLeft(ls, Nils: ListA[A])((z, h) => if (f(h)) Cons(h, z) else z)

  def filter_2[A](ls: ListA[A])(f: A => Boolean): ListA[A] = ls match {
    case Nils => Nils
    case Cons(h, t) => if (f(h)) Cons(h, filter(t)(f)) else filter(t)(f)
  }

  def forEach[A](ls: ListA[A])(f: A => Unit) = foldRightViaFoldLeft(ls, ())((_, h) => f(h))


  def flatMap[A, B](ls: ListA[A])(f: A => ListA[B]): ListA[B] = flatten(map(ls)(f))

  def filterViaFlatMap[A](ls: ListA[A])(f: A => Boolean): ListA[A] = flatMap(ls)(a => if (f(a)) Cons(a, tail(ls)) else tail(ls))

  def zipInts(ls: ListA[Int], l: ListA[Int]): ListA[Int] = ls match {
    case Nils => Nils
    case Cons(h1, t1) => l match {
      case Nils => Nils
      case Cons(h, t) => Cons(h + h1, zipInts(t1, t))
    }
  }

  def zipWith[A, B](ls: ListA[A], l: ListA[A])(f: (A, A) => B): ListA[B] = ls match {
    case Nils => Nils
    case Cons(h1, t1) => l match {
      case Nils => Nils
      case Cons(h, t) => val has = f(h, h1)
        Cons(has, zipWith(t, t1)(f))
    }
  }
}

sealed trait Tree[+A]
case class Leaf[A](value:A)extends Tree[A]
case class Branch[A](left:Tree[A], right:Tree[A]) extends Tree[A]



object Tree extends App {

  def count[A](t: Tree[A]): Integer = {
    t match {
      case Leaf(_) => 1
      case Branch(l, r) => count(l) + count(r)
    }
  }

  def depth[A](t: Tree[A]): Int = {
    t match {
      case Leaf(_) => 0
      case Branch(l, r) => Integer.max(depth(l) + 1, (depth(r) + 1))
    }
  }

  def maximum(t: Tree[Int]): Integer = {
    def go(t: Tree[Int]): Integer = {
      t match {
        case Leaf(x) => x
        case Branch(l, r) => maximum(l).max(maximum(r))
      }
    }

    go(t)
  }

  def map[A, B](t: Tree[A], f: A => B): Tree[B] = {
    t match {
      case Leaf(x) => Leaf(f(x))
      case Branch(l1, r1) => Branch(map_b(l1, f), map_b(r1, f))
    }
  }

  def map_b[A, B](t: Tree[A], f: A => B): Tree[B] = {
    t match {
      case Leaf(x) => Leaf(f(x))
      case Branch(l1, r1) => Branch(map(l1, f), map(r1, f))
    }
  }
}



