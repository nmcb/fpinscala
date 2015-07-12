package fpinscala.laziness

trait Stream[+A] {

  import Stream._

  def toListRec: List[A] = this match {
    case Cons(h, t) => h() :: t().toListRec
    case _          => Nil
  }

  def toList: List[A] = {
    def loop(s: Stream[A], a: List[A]): List[A] = s match {
      case Empty      => a
      case Cons(h, t) => loop(t(), h() :: a)
    }
    loop(this, Nil).reverse
  }

  def foldRight[B](z: => B)(f: (A, => B) => B): B = // The arrow `=>` in front of the argument type `B` means that the function `f` takes its second argument by name and may choose not to evaluate it.
    this match {
      case Cons(h, t) => f(h(), t().foldRight(z)(f)) // If `f` doesn't evaluate its second argument, the recursion never occurs.
      case _          => z
    }

  def exists(p: A => Boolean): Boolean =
    foldRight(false)((a, b) => p(a) || b) // Here `b` is the unevaluated recursive step that folds the tail of the stream. If `p(a)` returns `true`, `b` will never be evaluated and the computation terminates early.

  @annotation.tailrec
  final def find(f: A => Boolean): Option[A] = this match {
    case Empty      => None
    case Cons(h, t) => if (f(h())) Some(h()) else t().find(f)
  }

  def take(n: Int): Stream[A] = this match {
    case Cons(h, t) if n > 1  => cons(h(), t().take(n - 1))
    case Cons(h, _) if n == 1 => cons(h(), empty)
    case _                    => empty
  }

  def drop(n: Int): Stream[A] = this match {
    case Cons(h, t) if n > 0 => t().drop(n - 1)
    case _                   => this
  }

  def takeWhile(p: A => Boolean): Stream[A] = this match {
    case Cons(h, t) if p(h()) => cons(h(), t().takeWhile(p))
    case _                    => empty
  }

  def takeWhileByFold(p: A => Boolean): Stream[A] =
    foldRight(empty[A])((a, b) => { if (p(a)) cons(a, b) else empty })

  def forAll(p: A => Boolean): Boolean =
    foldRight(true)((a, b) => p(a) && b)

  def headOption: Option[A] =
    foldRight(None: Option[A])((a, _) => Some(a))

  def map[B](f: A => B): Stream[B] =
    foldRight(empty[B])((a, b) => cons(f(a), b))

  def filter(p: A => Boolean): Stream[A] =
    foldRight(empty[A])((a, b) => { if (p(a)) cons(a, b) else b })

  def append[T >: A](s: => Stream[T]): Stream[T] =
    foldRight(s)((a, b) => cons(a, b))

  def flatMap[B](f: A => Stream[B]): Stream[B] =
    foldRight(empty[B])((a, b) => f(a) append b)

  def startsWith[B](s: Stream[B]): Boolean = (this, s) match {
    case (_, Empty)                                     => true
    case (Cons(h1, t1), Cons(h2, t2)) if (h1() == h2()) => t1().startsWith(t2())
    case _                                              => false
  }

  // from answers, interwoven:
  // - the zipping,
  // - the taking while s not exhausted (breaking out early),
  // - and the validation that each (optional, including this) head equals the other
  def startsWithInterwoven[A](s: Stream[A]): Boolean =
    zipAll(s).takeWhile(!_._2.isEmpty) forAll {
      case (h,h2) => h == h2
    }

  def mapByUnfold[B](f: A => B): Stream[B] = unfold(this) {
    case Cons(h, t) => Some((f(h()), t()))
    case _          => None
  }

  def takeByUnfold(n: Int) = unfold((this, n)) {
    case (Cons(h, t), i) if i == 0 => Some((h(), (empty, i - 1)))
    case (Cons(h, t), i) if i > 0  => Some((h(), (t(), i - 1)))
    case _                         => None
  }

  def takeWhileByUnfold(f: A => Boolean) = unfold(this) {
    case Cons(h, t) if f(h()) => Some((h(), t()))
    case _                    => None
  }
  def zip[B](s2: Stream[B]): Stream[(A, B)] = unfold((this, s2)) {
    case (Cons(h1, t1), Cons(h2, t2)) => Some((h1(), h2()), (t1(), t2()))
    case _                            => None
  }
  def zipByZipWith[B](s2: Stream[B]): Stream[(A, B)] = zipWith(s2)((_, _))

  def zipWith[B, C](s2: Stream[B])(f: (A, B) => C): Stream[C] = unfold((this, s2)) {
    case (Cons(h1, t1), Cons(h2, t2)) => Some(f(h1(), h2()), (t1(), t2()))
    case _                            => None
  }
  def zipAll[B](s2: Stream[B]): Stream[(Option[A], Option[B])] = unfold((this, s2)) {
    case (Empty, Empty)               => None
    case (Cons(h1, t1), Empty)        => Some(((Some(h1()), Option.empty[B]), (t1(), empty[B])))
    case (Empty, Cons(h2, t2))        => Some(((Option.empty[A], Some(h2())), (empty[A], t2())))
    case (Cons(h1, t1), Cons(h2, t2)) => Some(((Some(h1()), Some(h2())), (t1(), t2())))
  }

}
case object Empty extends Stream[Nothing]
case class Cons[+A](h: () => A, t: () => Stream[A]) extends Stream[A]

object Stream {
  val ones: Stream[Int] = Stream.cons(1, ones)
  def empty[A]: Stream[A] = Empty

  def apply[A](as: A*): Stream[A] =
    if (as.isEmpty) empty
    else cons(as.head, apply(as.tail: _*))

  def constant[A](a: A): Stream[A] = Stream.cons(a, constant(a))

  def from(n: Int): Stream[Int] = Stream.cons(n, from(n + 1))

  def fibs: Stream[Int] = {
    def go(i1: Int, i2: Int): Stream[Int] = cons(i1, go(i1, i1 + i2))
    go(0, 1)
  }
  def cons[A](hd: => A, tl: => Stream[A]): Stream[A] = {
    lazy val head = hd
    lazy val tail = tl
    Cons(() => head, () => tail)
  }
  def unfold[A, S](z: S)(f: S => Option[(A, S)]): Stream[A] =
    f(z) match {
      case Some((a: A, s: S)) => cons(a, unfold(s)(f))
      case None               => empty
    }

  def onesByUnfold: Stream[Int] = unfold(Unit)(_ => Some((1, Unit)))

  def constantByUnfold(i: Int) = unfold(i)(_ => Some((i, i)))

  def fromByUnfold(n: Int) = unfold(n)(_ => Some((n, n + 1)))

  def fibsByUnfold = unfold((0, 1)) { case (a, b) => Some((a, (b, a + b))) }
}