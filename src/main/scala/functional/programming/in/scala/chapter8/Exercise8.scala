package functional.programming.in.scala.chapter8

object Exercise8 {
    case class Gen[A](sample: State[RNG,A]) {
        def flatMap[B](f: A => Gen[B]): Gen[B] =
            Gen(sample.flatMap(f.andThen(_.sample)))

        def listOfN(size: Gen[Int]): Gen[List[A]] =
            size.flatMap(i => Gen.listOfN(i, this))
    }

    case object Gen {
        def choose(start: Int, stopExclusive: Int): Gen[Int] =
            Gen(Rand.nonNegativeLessThan(stopExclusive - start).map(_ + start))

        def unit[A](a: => A): Gen[A] =
            Gen(State.unit(a))

        def boolean: Gen[Boolean] =
            Gen(Rand.int.map(i => i % 2 == 0))

        def listOfN[A](n: Int, g: Gen[A]): Gen[List[A]] =
            Gen(State.sequence(List.fill(n)(g.sample)))

        def union[A](g1: Gen[A], g2: Gen[A]): Gen[A] =
            boolean.flatMap(cond => if (cond) g1 else g2)

        def weighted[A](g1: (Gen[A], Double), g2: (Gen[A], Double)): Gen[A] =
            Gen(Rand.nonNegativeInt.map(_ < (g1._2 / (g1._2 + g2._2) * Int.MaxValue).toInt))
                    .flatMap(cond => if (cond) g1._1 else g2._1)
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
