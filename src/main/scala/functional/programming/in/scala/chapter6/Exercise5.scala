package functional.programming.in.scala.chapter6

object Exercise5 {
    type Rand[+A] = RNG => (A, RNG)

    def unit[A](a: A): Rand[A] =
        rng => (a, rng)

    def map[A,B](s: Rand[A])(f: A => B): Rand[B] = rng => {
        val (a, rng2) = s(rng)
        (f(a), rng2)
    }

    def int: Rand[Int] =
        _.nextInt

    def nonNegativeInt: Rand[Int] =
        map(int)(i => if (i < 0) -(i + 1) else i)

    def double: Rand[Double] =
        map(nonNegativeInt)(_ / (Int.MaxValue.toDouble + 1))

    sealed trait RNG {
        def nextInt: (Int, RNG)
    }

    case class SimpleRNG(seed: Long) extends RNG {
        override def nextInt: (Int, RNG) = {
            val newSeed = (seed * 0x5DEECE66DL + 0xBL) & 0xFFFFFFFFFFFFL
            val nextRNG = SimpleRNG(newSeed)
            val n = (newSeed >>> 16).toInt

            (n, nextRNG)
        }
    }
}
