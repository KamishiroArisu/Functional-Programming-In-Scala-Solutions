package functional.programming.in.scala.chapter8

object Exercise15 {
    case class SizedSGen[A](forSize: Int => Gen[A]) extends SGen[A] {
        override def map[B](f: (A) => B): SGen[B] =
            SizedSGen(forSize.andThen(_.map(f)))

        override def flatMap[B](f: (A) => Gen[B]): SGen[B] =
            SizedSGen(forSize.andThen(_.flatMap(f)))
    }

    case class UnsizedSGen[A](gen: Gen[A]) extends SGen[A] {
        override def map[B](f: (A) => B): SGen[B] =
            UnsizedSGen(gen.map(f))

        override def flatMap[B](f: (A) => Gen[B]): SGen[B] =
            UnsizedSGen(gen.flatMap(f))
    }

    def check(p: => Boolean): Prop = Prop((_, _, _) => if (p) Proved else Falsified("()", 0))

    def forAll[A](sg: SGen[A])(f: A => Boolean): Prop = sg match {
        case SizedSGen(forSize) => forAll(forSize)(f)
        case UnsizedSGen(gen) => forAll(gen)(f)
    }

    def forAll[A](g: Int => Gen[A])(f: A => Boolean): Prop = Prop {
        (max, n, rng) =>
            val casesPerSize = (n + (max - 1)) / max
            val props: Stream[Prop] = Stream.from(0).take((n min max) + 1).map(i => forAll(g(i))(f))
            val prop: Prop = props.map(p => Prop {
                (max, _, rng) => p.run(max, casesPerSize, rng)
            }).toList.reduce(_ && _)
            val result = prop.run(max, n, rng)
            result match {
                case Proved => Exhausted
                case x => x
            }
    }

    def forAll[A](as: Gen[A])(f: A => Boolean): Prop = Prop {
        def tryRandom(n: Int, r: RNG): Result = {
            randomStream(as)(r).zip(Stream.from(0)).take(n).map {
                case (a, i) => try {
                    if (f(a)) Passed else Falsified(a.toString, i)
                } catch {
                    case e: Exception => Falsified(buildMsg(a, e), i)
                }
            }.find(_.isFalsified).getOrElse(Passed)
        }

        def tryProve(n: Int): Result = {
            def go(i: Int, j: Int, s: Stream[Option[A]]): Result = {
                if (i == j)
                    Undefined(i)
                else s match {
                    case head #:: tail => head match {
                        case Some(h) =>
                            try {
                                if (f(h))
                                    go(i + 1, j, tail)
                                else
                                    Falsified(h.toString, i)
                            } catch {
                                case e: Exception => Falsified(buildMsg(h, e), i)
                            }
                        case None => Undefined(i)
                    }
                    case _ => Proved
                }
            }

            go(0, n, as.exhaustive)
        }

        (_, n, rng) => {
            tryProve(n / 2) match {
                case Proved => Proved
                case f@Falsified(_, _) => f
                case Undefined(i) => tryRandom(n - i, rng)
            }
        }
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
                case f: Falsified => f
                case s => p.tagSuccess(s, n).run(max, n, rng)
            }
        }

        def ||(p: Prop): Prop = Prop {
            (max, n, rng) => Prop.this.run(max, n, rng) match {
                case Falsified(f, s) => p.tagFalsified(f, s).run(max, n, rng)
                case s => s
            }
        }

        def tagSuccess(r: Result, s: SuccessCount): Prop = Prop {
            (max, n, rng) => (r, Prop.this.run(max, n, rng)) match {
                case (_, Falsified(failure, successes)) => Falsified(failure, s + successes)
                case (_, Passed) | (Passed, _) => Passed
                case (_, Exhausted) | (Exhausted, _) => Exhausted
                case (Proved, Proved) => Proved
            }
        }

        def tagFalsified(f: FailedCase, s: SuccessCount): Prop = Prop {
            (max, n, rng) => Prop.this.run(max, n, rng) match {
                case Falsified(failure, successes) => Falsified(f + "\n" + failure, s + successes)
                case x => x
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
                case Proved =>
                    println(s"+ OK, proved property.")
                case Exhausted =>
                    println(s"+ OK, counterexample is not exist in given size.")
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

    case object Proved extends Result {
        def isFalsified: Boolean = false
    }

    case object Exhausted extends Result {
        def isFalsified: Boolean = false
    }

    case class Undefined(successes: SuccessCount) extends Result {
        def isFalsified: Boolean = false
    }

    case class Gen[A](sample: State[RNG,A], exhaustive: Stream[Option[A]]) {
        def map[B](f: A => B): Gen[B] =
            Gen(sample.map(f), exhaustive.map(_.map(f)))

        def flatMap[B](f: A => Gen[B]): Gen[B] =
            Gen(sample.flatMap(f.andThen(_.sample)), exhaustive.flatMap({
                case Some(a) => f(a).exhaustive
                case None => Stream(None)
            }))

        def listOfN(size: Gen[Int]): Gen[List[A]] =
            size.flatMap(i => Gen.listOfN(i, this))

        def unsized: SGen[A] =
            UnsizedSGen(this)
    }

    case object Gen {
        def choose(start: Int, stopExclusive: Int): Gen[Int] =
            Gen(Rand.nonNegativeLessThan(stopExclusive - start).map(_ + start),
                Stream.from(start).take(stopExclusive - start).map(Option(_)))

        def unit[A](a: => A): Gen[A] =
            Gen(State.unit(a), Stream(Some(a)))

        def boolean: Gen[Boolean] =
            Gen(Rand.int.map(i => i % 2 == 0), Stream(Some(true), Some(false)))

        def listOfN[A](n: Int, g: Gen[A]): Gen[List[A]] =
            Gen(State.sequence(List.fill(n)(g.sample)),
                product(Stream.continually(g.exhaustive).take(n))
                        .map(x => Some(x.toList.filter(_.isDefined).map(_.get))))

        def union[A](g1: Gen[A], g2: Gen[A]): Gen[A] =
            boolean.flatMap(cond => if (cond) g1 else g2)

        def weighted[A](g1: (Gen[A], Double), g2: (Gen[A], Double)): Gen[A] = {
            val sample = Rand.nonNegativeInt.map(_ < (g1._2 / (g1._2 + g2._2) * Int.MaxValue).toInt)
            Gen(sample, randomStream(Gen(sample, Stream(None)))(SimpleRNG(42)).map(Some(_)))
                    .flatMap(cond => if (cond) g1._1 else g2._1)
        }
    }

    trait SGen[A] {
        def map[B](f: A => B): SGen[B]
        def flatMap[B](f: A => Gen[B]): SGen[B]
    }

    case object SGen {
        def listOf[A](g: Gen[A]): SGen[List[A]] =
            SizedSGen(i => Gen.listOfN(i, g))

        def listOf1[A](g: Gen[A]): SGen[List[A]] =
            SizedSGen(i => Gen.listOfN(i max 1, g))
    }

    def product[A](ssa: Stream[Stream[A]]): Stream[Stream[A]] =
        ssa.foldRight(Stream(Stream[A]()))((sa, z) => sa.flatMap(a => z.map(a #:: _)))

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
