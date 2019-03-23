package functional.programming.in.scala.chapter6

object Exercise8 {
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

    def map2[A,B,C](ra: Rand[A], rb: Rand[B])(f: (A, B) => C): Rand[C] = rng => {
        val (a, rng2) = ra(rng)
        val (b, rng3) = rb(rng2)
        (f(a, b), rng3)
    }

    def sequence[A](fs: List[Rand[A]]): Rand[List[A]] =
        fs.foldRight(unit(List[A]()))(map2(_, _)(_ :: _))

    def ints(count: Int): Rand[List[Int]] =
        sequence(List.fill(count)(int))

    def flatMap[A,B](f: Rand[A])(g: A => Rand[B]): Rand[B] = rng => {
        val (a, rng2) = f(rng)
        g(a)(rng2)
    }

    def nonNegativeLessThan(n: Int): Rand[Int] =
        flatMap(nonNegativeInt)((i: Int) => {
            val mod = i % n
            if (i + (n - 1) - mod >= 0)
                unit(mod)
            else
                nonNegativeLessThan(n)
        })

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
