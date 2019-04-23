package functional.programming.in.scala.chapter8

object Exercise14 {
    def sortedProp: Prop = {
        val gen = Gen.choose(-10, 100)
        val sgen = SGen.listOf(gen)

        val prop1 = forAll(sgen)(ns => ns.sorted.forall(e => ns contains e))
        val prop2 = forAll(sgen)(ns => ns.sorted.size == ns.size)
        val prop3 = forAll(sgen)(ns => {
            val nssorted = ns.sorted
            if (nssorted.size <= 1) true else (0 until (nssorted.size - 1)).forall(i => nssorted(i) <= nssorted(i + 1))
        })

        prop1 && prop2 && prop3
    }

    case class SGen[A](forSize: Int => Gen[A]) {
        def apply(size: Int): Gen[A] =
            forSize(size)

        def map[B](f: A => B): SGen[B] =
            SGen(i => forSize(i).map(f))

        def flatMap[B](f: A => SGen[B]): SGen[B] =
            SGen(i => forSize(i).flatMap(f.andThen(_.forSize(i))))
    }

    case object SGen {
        def listOf[A](g: Gen[A]): SGen[List[A]] =
            SGen(i => Gen.listOfN(i, g))

        def listOf1[A](g: Gen[A]): SGen[List[A]] =
            SGen(i => Gen.listOfN(i max 1, g))
    }

    def forAll[A](g: SGen[A])(f: A => Boolean): Prop =
        forAll(g(_))(f)

    def forAll[A](g: Int => Gen[A])(f: A => Boolean): Prop = Prop {
        (max, n, rng) =>
            val casesPerSize = (n + (max - 1)) / max
            val props: Stream[Prop] = Stream.from(0).take((n min max) + 1).map(i => forAll(g(i))(f))
            val prop: Prop = props.map(p => Prop {
                (max, _, rng) => p.run(max, casesPerSize, rng)
            }).toList.reduce(_ && _)
            prop.run(max, n, rng)
    }

    def forAll[A](as: Gen[A])(f: A => Boolean): Prop = Prop {
        (_, n, rng) => randomStream(as)(rng).zip(Stream.from(0)).take(n).map {
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

    case class Prop(run: (MaxSize, TestCases, RNG) => Result) {
        def &&(p: Prop): Prop = Prop {
            (max, n, rng) => Prop.this.run(max, n, rng) match {
                case Passed => p.tagPassedAnd(n).run(max, n, rng)
                case f: Falsified => f
            }
        }

        def ||(p: Prop): Prop = Prop {
            (max, n, rng) => Prop.this.run(max, n, rng) match {
                case Falsified(f, s) => p.tagFalsifiedOr(f, s).run(max, n, rng)
                case Passed => Passed
            }
        }

        def tagPassedAnd(s: SuccessCount): Prop = Prop {
            (max, n, rng) => Prop.this.run(max, n, rng) match {
                case Falsified(failure, successes) => Falsified(failure, s + successes)
                case Passed => Passed
            }
        }

        def tagFalsifiedOr(f: FailedCase, s: SuccessCount): Prop = Prop {
            (max, n, rng) => Prop.this.run(max, n, rng) match {
                case Falsified(failure, successes) => Falsified(f + "\n" + failure, s + successes)
                case Passed => Passed
            }
        }
    }

    case object Prop {
        def run(p: Prop, maxSize: Int = 100, testCases: Int = 100, rng: RNG = SimpleRNG(System.currentTimeMillis)): Unit =
            p.run(maxSize, testCases, rng) match {
                case Falsified(msg, n) =>
                    println(s"! Falsified after $n passed tests:\n $msg")
                case Passed =>
                    println(s"+ OK, passed $testCases tests.")
            }
    }

    type MaxSize = Int
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
