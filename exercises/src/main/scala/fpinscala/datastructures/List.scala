package fpinscala.datastructures
import scala.collection.mutable.ListBuffer

sealed trait List[+A]
// `List` data type, parameterized on a type, `A`
case object Nil extends List[Nothing]
// A `List` data constructor representing the empty list
case class Cons[+A](head: A, tail: List[A]) extends List[A]
// Another data constructor, representing nonempty lists. Note that `tail` is another `List[A]`, which may be `Nil` or another `Cons`.

object List {
  // `List` companion object. Contains functions for creating and working with lists.
  def sum(ints: List[Int]): Int = ints match {
    // A function that uses pattern matching to add up a list of integers
    case Nil         => 0 // The sum of the empty list is 0.
    case Cons(x, xs) => x + sum(xs) // The sum of a list starting with `x` is `x` plus the sum of the rest of the list.
  }

  def product(ds: List[Double]): Double = ds match {
    case Nil          => 1.0
    case Cons(0.0, _) => 0.0
    case Cons(x, xs)  => x * product(xs)
  }

  def apply[A](as: A*): List[A] = // Variadic function syntax
    if (as.isEmpty) Nil
    else Cons(as.head, apply(as.tail: _*))

  val x = List(1, 2, 3, 4, 5) match {
    case Cons(x, Cons(2, Cons(4, _)))          => x
    case Nil                                   => 42
    case Cons(x, Cons(y, Cons(3, Cons(4, _)))) => x + y
    case Cons(h, t)                            => h + sum(t)
    case _                                     => 101
  }

  def append[A](a1: List[A], a2: List[A]): List[A] =
    a1 match {
      case Nil        => a2
      case Cons(h, t) => Cons(h, append(t, a2))
    }

  def sum2(ns: List[Int]) =
    foldRight(ns, 0)((x, y) => x + y)

  def product2(ns: List[Double]) =
    foldRight(ns, 1.0)(_ * _)

  def sum3(ns: List[Int]) =
    foldLeft(ns, 0)(_ + _)

  def product3(ns: List[Double]) =
    foldLeft(ns, 1.0)(_ * _)

  def tail[A](l: List[A]): List[A] = l match {
    case Nil         => Nil
    case Cons(x, xs) => xs
  }

  def tail2[A](l: List[A]): List[A] = drop(l, 1)

  def setHead[A](l: List[A], h: A): List[A] = l match {
    case Nil         => Cons(h, Nil)
    case Cons(y, ys) => Cons(h, ys)
  }

  def drop[A](l: List[A], n: Int): List[A] = {
    if (n <= 0) l
    else l match {
      case Nil         => Nil
      case Cons(x, xs) => drop(xs, n - 1)
    }
  }

  def dropWhile[A](l: List[A], f: A => Boolean): List[A] = l match {
    case Cons(x, xs) if f(x) => dropWhile(xs, f)
    case _                   => l
  }

  def dropWhileCurried[A](l: List[A])(f: A => Boolean): List[A] = l match {
    case Cons(x, xs) if f(x) => dropWhile(xs, f)
    case _                   => l
  }

  // O(n)
  def init[A](l: List[A]): List[A] = l match {
    case Nil          => Nil
    case Cons(x, Nil) => l
    case Cons(x, xs)  => Cons(x, init(xs))
  }

  // 3.7  I guess not, perhaps pass a predicate to foldRight?

  // 3.8  Seems that foldRight with list constructors replaces
  //      the cons "operators" with cons, and the nil with nil.

  def length[A](l: List[A]): Int = foldRight(l, 0)((_, acc) => acc + 1)

  def length2[A](l: List[A]): Int = foldLeft(l, 0)((acc, _) => acc + 1)

  def foldRight[A, B](as: List[A], z: B)(f: (A, B) => B): B = as match {
    case Nil         => z
    case Cons(x, xs) => f(x, foldRight(xs, z)(f))
  }

  @annotation.tailrec
  def foldLeft[A, B](l: List[A], z: B)(f: (B, A) => B): B = l match {
    case Nil         => z
    case Cons(a, as) => foldLeft(as, f(z, a))(f)
  }

  def reverse[A](l: List[A]): List[A] =
    foldLeft(l, Nil: List[A])((acc, a) => Cons(a, acc))

  def foldRightByFoldLeft[A, B](l: List[A], z: B)(f: (A, B) => B): B =
    foldLeft(reverse(l), z)((b, a) => f(a, b))

  def foldRightByFoldLeft2[A, B](l: List[A], z: B)(f: (A, B) => B): B =
    foldLeft(l, (b: B) => b)((g, a) => b => g(f(a, b)))(z)

  def appendByFoldRight[A](l: List[A], m: List[A]): List[A] =
    foldRight(l, m)(Cons(_, _))

  def concat[A](ll: List[List[A]]): List[A] =
    foldRight(ll, Nil: List[A])(append)

  def map[A, B](as: List[A])(f: A => B): List[B] =
    foldRight(as, Nil: List[B])((a, bs) => Cons(f(a), bs))

  def mapimp[A, B](as: List[A])(f: A => B): List[B] = {
    val buf = new ListBuffer[B]
    def loop(as: List[A]): Unit = as match {
      case Nil        => ()
      case Cons(h, t) => buf += f(h); loop(t)
    }
    loop(as)
    // converting from scala's buffer list to our list.
    List(buf.toList: _*)
  }

  def addone(is: List[Int]): List[Int] =
    foldRight(is, Nil: List[Int])((i, m) => Cons(i + 1, m))

  def tostr(ds: List[Double]): List[String] =
    foldRight(ds, Nil: List[String])((d, m) => Cons(d.toString, m))

  def filter[A](as: List[A])(f: A => Boolean): List[A] =
    foldRight(as, Nil: List[A])((h, t) => if (f(h)) Cons(h, t) else t)

  def even(as: List[Int]) = filter(as)(_ % 2 == 0)

  val _even = even(List(1, 2, 3, 4, 5, 6, 7, 8))

  def flatMap[A, B](as: List[A])(f: A => List[B]): List[B] =
    concat(map(as)(f))

  def filterByFlatMap[A](as: List[A])(f: A => Boolean): List[A] =
    flatMap(as)(a => if (f(a)) List(a) else Nil)

  def zip[A, B, C](as: List[A], bs: List[B])(f: (A, B) => C): List[C] = (as, bs) match {
    case (Nil, _)                   => Nil
    case (_, Nil)                   => Nil
    case (Cons(a, ta), Cons(b, tb)) => Cons(f(a, b), zip(ta, tb)(f))
  }

  def hasSubsequence[A](sup: List[A], sub: List[A]): Boolean = {
    def hasInit(as: List[A], is: List[A]): Boolean = (as, is) match {
      case (_, Nil)                               => true
      case (Cons(a, at), Cons(i, it)) if (a == i) => hasInit(at, it)
    }
    sub match {
      case Nil                             => false
      case Cons(h, t) if hasInit(sup, sub) => true
      case Cons(h, t)                      => hasSubsequence(t, sub)
    }
  }
}