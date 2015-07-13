package fpinscala.state

import scala.annotation.tailrec

trait RNG {
  def nextInt: (Int, RNG) // Should generate a random `Int`. We'll later define other functions in terms of `nextInt`.
}

object RNG {
  // NB - this was called SimpleRNG in the book text

  type Rand[+A] = RNG => (A, RNG)
  val randInt: Rand[Int] = _.nextInt
  val randNonNegative: Rand[Int] = map(randInt)(i => if (i < 0) -(i + 1) else i)
  val randNonNegativeEven: Rand[Int] = map(randNonNegative)(i => i - i % 2)
  val randomBoolean: Rand[Boolean] = map(randInt)(i => i % 2 == 0)
  val randDouble: Rand[Double] = map(randInt)(i => i / (Int.MaxValue.toDouble + 1))
  val randIntDouble: Rand[(Int, Double)] = both(randInt, randDouble)
  val randDoubleInt: Rand[(Double, Int)] = both(randDouble, randInt)
  def unit[A](a: A): Rand[A] = rng => (a, rng)
  def map[A, B](s: Rand[A])(f: A => B): Rand[B] = {
    rng => {
      val (a, rng2) = s(rng)
      (f(a), rng2)
    }
  }
  def nonNegativeInt(rng: RNG): (Int, RNG) = {
    val (i, r) = rng.nextInt
    (if (i < 0) -(i + 1) else i, r)
  }
  def boolean(rng: RNG): (Boolean, RNG) = {
    val (i, r) = rng.nextInt
    (i % 2 == 0, r)
  }
  def doubleInt(rng: RNG): ((Double, Int), RNG) = {
    val ((n, d), r) = intDouble(rng)
    ((d, n), r)
  }
  def intDouble(rng: RNG): ((Int, Double), RNG) = {
    val (n, r0) = rng.nextInt
    val (d, r1) = double(r0)
    ((n, d), r1)
  }
  def double3(rng: RNG): ((Double, Double, Double), RNG) = {
    val (d0, r0) = double(rng)
    val (d1, r1) = double(r0)
    val (d2, r2) = double(r1)
    ((d0, d1, d2), r2)
  }
  def double(rng: RNG): (Double, RNG) = {
    val (n, r) = rng.nextInt
    (n / (Int.MaxValue.toDouble + 1), r)
  }

  def ints(count: Int)(rng: RNG): (List[Int], RNG) = {
    @tailrec
    def loop(n: Int, r: RNG, a: List[Int]): (List[Int], RNG) = {
      if (n == 0) (a, r)
      else {
        val (i, r0) = r.nextInt
        loop(n - 1, r0, i :: a)
      }
    }
    loop(count, rng, List.empty)
  }
  def randInts(count: Int): Rand[List[Int]] = sequence(List.fill(count)(randInt))

  def map2[A, B, C](ra: Rand[A], rb: Rand[B])(f: (A, B) => C): Rand[C] = {
    rng => {
      val (a, rng0) = ra(rng)
      val (b, rng1) = rb(rng0)
      (f(a, b), rng1)
    }
  }

  def both[A,B](ra: Rand[A], rb: Rand[B]): Rand[(A,B)] = map2(ra, rb)((_, _))

  def sequence[A](fs: List[Rand[A]]): Rand[List[A]] = {
    fs.foldRight(unit(List.empty[A]))((f, acc) => map2(f, acc)(_ :: _))
  }

  def flatMap[A, B](f: Rand[A])(g: A => Rand[B]): Rand[B] = ???

  case class Simple(seed: Long) extends RNG {
    def nextInt: (Int, RNG) = {
      val newSeed = (seed * 0x5DEECE66DL + 0xBL) & 0xFFFFFFFFFFFFL // `&` is bitwise AND. We use the current seed to generate a new seed.
      val nextRNG = Simple(newSeed) // The next state, which is an `RNG` instance created from the new seed.
      val n = (newSeed >>> 16).toInt // `>>>` is right binary shift with zero fill. The value `n` is our new pseudo-random integer.
      (n, nextRNG) // The return value is a tuple containing both a pseudo-random integer and the next `RNG` state.
    }
  }
}

case class State[S, +A](run: S => (A, S)) {
  def map[B](f: A => B): State[S, B] =
    sys.error("todo")
  def map2[B, C](sb: State[S, B])(f: (A, B) => C): State[S, C] =
    sys.error("todo")
  def flatMap[B](f: A => State[S, B]): State[S, B] =
    sys.error("todo")
}

sealed trait Input
case object Coin extends Input
case object Turn extends Input

case class Machine(locked: Boolean, candies: Int, coins: Int)

object State {
  type Rand[A] = State[RNG, A]
  def simulateMachine(inputs: List[Input]): State[Machine, (Int, Int)] = ???
}
