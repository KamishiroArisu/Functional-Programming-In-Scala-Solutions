package functional.programming.in.scala.chapter8

object Exercise11 {
    case class SGen[A](forSize: Int => Gen[A]) {
        def map[B](f: A => B): SGen[B] =
            SGen(i => forSize(i).map(f))

        def flatMap[B](f: A => SGen[B]): SGen[B] =
            SGen(i => forSize(i).flatMap(f.andThen(_.forSize(i))))
    }

    def forAll[A](as: Gen[A])(f: A => Boolean): Prop = Prop {
        (n, rng) => randomStream(as)(rng).zip(Stream.from(0)).take(n).map {
            case (a, i) => try {
                if (f (a)) Passed else Falsified(a.toString, i)
            } catch {
                case e: Exception => Falsified(buildMsg(a, e), i)
            }
        }.find(_.isFalsified).getOrElse(Passed)
    }

    def randomStream[A](g: Gen[A])(r: RNG): Stream[A] = {
        val (a, rng) = g.sample.run(r)
        a #:: randomStream(g)(rng)
    }

    def buildMsg[A](s: A, e: Exception): String =
        s"test case: $s\n" +
                s"generated an exception: ${e.getMessage}\n" +
                s"stack trace:\n ${e.getStackTrace.mkString("\n")}"

    case class Prop(run: (TestCases, RNG) => Result) {
        def &&(p: Prop): Prop = Prop {
            (n, rng) => Prop.this.run(n, rng) match {
                case Passed => p.tagPassedAnd(n).run(n, rng)
                case f: Falsified => f
            }
        }

        def ||(p: Prop): Prop = Prop {
            (n, rng) => Prop.this.run(n, rng) match {
                case Falsified(f, s) => p.tagFalsifiedOr(f, s).run(n, rng)
                case Passed => Passed
            }
        }

        def tagPassedAnd(s: SuccessCount): Prop = Prop {
            (n, rng) => Prop.this.run(n, rng) match {
                case Falsified(failure, successes) => Falsified(failure, s + successes)
                case Passed => Passed
            }
        }

        def tagFalsifiedOr(f: FailedCase, s: SuccessCount): Prop = Prop {
            (n, rng) => Prop.this.run(n, rng) match {
                case Falsified(failure, successes) => Falsified(f + "\n" + failure, s + successes)
                case Passed => Passed
            }
        }
    }

    type TestCases = Int
    type FailedCase = String
    type SuccessCount = Int

    sealed trait Result {
        def isFalsified: Boolean
    }

    case object Passed extends Result {
        def isFalsified: Boolean = false
    }

    case class Falsified(failure: FailedCase, successes: SuccessCount) extends Result {
        def isFalsified: Boolean = true
    }

    case class Gen[A](sample: State[RNG,A]) {
        def map[B](f: A => B): Gen[B] =
            flatMap(f.andThen(b => Gen.unit(b)))

        def flatMap[B](f: A => Gen[B]): Gen[B] =
            Gen(sample.flatMap(f.andThen(_.sample)))

        def listOfN(size: Gen[Int]): Gen[List[A]] =
            size.flatMap(i => Gen.listOfN(i, this))

        def unsized: SGen[A] =
            SGen(_ => this)
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
