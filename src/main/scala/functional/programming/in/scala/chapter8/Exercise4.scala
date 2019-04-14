package functional.programming.in.scala.chapter8

object Exercise4 {
    case class Gen[A](sample: State[RNG,A])

    case object Gen {
        def choose(start: Int, stopExclusive: Int): Gen[Int] =
            Gen(Rand.nonNegativeLessThan(stopExclusive - start).map(_ + start))
    }

    case class State[S,+A](run: S => (A, S)) {
        def flatMap[B](f: A => State[S,B]): State[S,B] = State(s => {
            val (a, s2) = run(s)
            f(a).run(s2)
        })

        def map[B](f: A => B): State[S,B] =
            flatMap(a => State.unit(f(a)))

        def map2[B,C](sb: State[S,B])(f: (A, B) => C): State[S,C] =
            flatMap(a => sb.map(b => f(a, b)))
    }

    object State {
        def unit[S,A](a: A): State[S,A] = State(s => (a, s))

        def sequence[S,A](fs: List[State[S,A]]): State[S,List[A]] =
            fs.foldLeft(unit[S,List[A]](List[A]()))((z, sa) => z.map2(sa)((l, a) => a :: l)).map(_.reverse)
    }

    type Rand[A] = State[RNG, A]

    object Rand {
        def int: Rand[Int] = State(_.nextInt)

        def ints(count: Int): Rand[List[Int]] =
            State.sequence(List.fill(count)(int))

        def unit[A](a: A): Rand[A] =
            State(rng => (a, rng))

        def nonNegativeInt: Rand[Int] =
            int.map(i => if (i < 0) -(i + 1) else i)

        def nonNegativeLessThan(n: Int): Rand[Int] =
            nonNegativeInt.flatMap((i: Int) => {
                val mod = i % n
                if (i + (n - 1) - mod >= 0)
                    unit(mod)
                else
                    nonNegativeLessThan(n)
            })
    }

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
