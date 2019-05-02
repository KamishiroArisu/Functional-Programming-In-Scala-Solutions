package functional.programming.in.scala.chapter8

import java.io.ByteArrayOutputStream

import org.scalatest.FunSuite
import functional.programming.in.scala.chapter8.Exercise15._

class Exercise15Test extends FunSuite {
    test("Prop.&&") {
        val passed = Prop((_, _, _) => Passed)
        val falsified = Prop((_, _, _) => Falsified("test", 0))
        val proved = Prop((_, _, _) => Proved)
        val exhausted = Prop((_, _, _) => Exhausted)

        assert((passed && passed).run(1, 1, SimpleRNG(42)) == Passed)
        assert((passed && falsified).run(1, 1, SimpleRNG(42)) == Falsified("test", 1))

        assert((passed && proved).run(1, 1, SimpleRNG(42)) == Passed)
        assert((proved && passed).run(1, 1, SimpleRNG(42)) == Passed)

        assert((passed && exhausted).run(1, 1, SimpleRNG(42)) == Passed)
        assert((exhausted && passed).run(1, 1, SimpleRNG(42)) == Passed)

        assert((proved && falsified).run(1, 1, SimpleRNG(42)) == Falsified("test", 1))
        assert((falsified && passed).run(1, 1, SimpleRNG(42)) == Falsified("test", 0))
        assert((falsified && proved).run(1, 1, SimpleRNG(42)) == Falsified("test", 0))
        assert((falsified && exhausted).run(1, 1, SimpleRNG(42)) == Falsified("test", 0))

        assert((falsified && falsified).run(1, 1, SimpleRNG(42)) == Falsified("test", 0))
    }

    test("Prop.||") {
        val passed = Prop((_, _, _) => Passed)
        val falsified = Prop((_, _, _) => Falsified("test", 0))
        val proved = Prop((_, _, _) => Proved)
        val exhausted = Prop((_, _, _) => Exhausted)

        assert((passed || passed).run(1, 1, SimpleRNG(42)) == Passed)
        assert((passed || falsified).run(1, 1, SimpleRNG(42)) == Passed)

        assert((passed || proved).run(1, 1, SimpleRNG(42)) == Passed)
        assert((proved || passed).run(1, 1, SimpleRNG(42)) == Proved)

        assert((passed || exhausted).run(1, 1, SimpleRNG(42)) == Passed)
        assert((exhausted || passed).run(1, 1, SimpleRNG(42)) == Exhausted)

        assert((proved || falsified).run(1, 1, SimpleRNG(42)) == Proved)
        assert((falsified || passed).run(1, 1, SimpleRNG(42)) == Passed)
        assert((falsified || proved).run(1, 1, SimpleRNG(42)) == Proved)
        assert((falsified || exhausted).run(1, 1, SimpleRNG(42)) == Exhausted)

        assert((falsified || falsified).run(1, 1, SimpleRNG(42)) == Falsified("test\ntest", 0))
    }

    test("choose") {
        val r = SimpleRNG(42)
        val (i1, r1) = Gen.choose(1, 10).sample.run(r)
        val (i2, r2) = Gen.choose(0, 100).sample.run(r1)
        val (i3, _) = Gen.choose(-1, 10).sample.run(r2)

        assert(1 until 10 contains i1)
        assert(0 until 100 contains i2)
        assert(-1 until 10 contains i3)
    }

    test("unit") {
        val r = SimpleRNG(42)
        val g = Gen.unit(12)
        val (i1, r1) = g.sample.run(r)
        val (i2, r2) = g.sample.run(r1)
        val (i3, _) = g.sample.run(r2)

        assert(i1 == 12)
        assert(i2 == 12)
        assert(i3 == 12)
    }

    test("boolean") {
        val r = SimpleRNG(42)
        assert(!Gen.boolean.sample.run(r)._1)
    }

    test("listOfN(Int, Gen[A])") {
        val r = SimpleRNG(42)
        val g = Gen.listOfN(10, Gen.unit(43))
        val (list, _) = g.sample.run(r)
        assert(list.length == 10)
        assert(list.forall(_ == 43))
    }

    test("Gen.flatMap") {
        assert(Gen.unit(4).flatMap(i => Gen.unit(i * i)).sample.run(SimpleRNG(42))._1 == 16)
        assert(21 until 30 contains Gen.choose(1, 10).flatMap(i => Gen.unit(i + 20)).sample.run(SimpleRNG(42))._1)
    }

    test("listOfN(Gen[Int])") {
        val list = Gen.unit(8).listOfN(Gen.unit(5)).sample.run(SimpleRNG(42))._1
        assert(list.length == 5)
        assert(list.forall(_ == 8))

        val list2 = Gen.choose(1, 4).listOfN(Gen.choose(5, 8)).sample.run(SimpleRNG(42))._1
        assert(5 until 8 contains list2.length)
        assert(list2.forall(e => 1 until 4 contains e))
    }

    test("union") {
        val g1 = Gen.unit(0)
        val g2 = Gen.unit(1)
        val list = Gen.union(g1, g2).listOfN(Gen.unit(1000)).sample.run(SimpleRNG(42))._1
        assert(list.forall(e => e == 0 || e == 1))
        assert(450 until 550 contains list.sum)
    }

    test("weighted") {
        val g1 = (Gen.unit(0), 0.8)
        val g2 = (Gen.unit(1), 0.2)
        val list = Gen.weighted(g1, g2).listOfN(Gen.unit(1000)).sample.run(SimpleRNG(42))._1
        assert(list.forall(e => e == 0 || e == 1))
        assert(150 until 250 contains list.sum)
    }

    test("unsized") {
        val sg = Gen.unit(3).unsized

        assert(forAll(sg)(_ == 3).run(10, 10, SimpleRNG(42)) == Proved)
        assert(forAll(sg)(_ == 3).run(100, 10, SimpleRNG(42)) == Proved)
    }

    test("SGen.map") {
        val gen = Gen.unit(24)
        val sgen = gen.unsized.map(_ + 12)
        assert(forAll(sgen)(_ == 36).run(10, 10, SimpleRNG(42)) == Proved)
    }

    test("SGen.flatMap") {
        val gen = Gen.unit(24)
        val sgen = gen.unsized
        assert(forAll(sgen.flatMap(i => Gen.unit(i + 12)))(_ == 36).run(10, 10, SimpleRNG(42)) == Proved)
    }

    test("listOf") {
        val sgen = SGen.listOf(Gen.union(Gen.unit(0), Gen.unit(1)))
        assert(forAll(sgen)(list => !list.exists(e => e != 0 && e != 1)).run(10, 10, SimpleRNG(42)) == Passed)
    }

    test("listOf1") {
        val smallInt = Gen.choose(-10, 10)
        val maxProp = forAll(SGen.listOf1(smallInt)) { ns =>
            val max = ns.max
            !ns.exists(_ > max)
        }
        val stream = new ByteArrayOutputStream()
        Console.withOut(stream) {
            Prop.run(maxProp)
            assert(stream.toString contains "OK")
        }
    }

    test("check") {
        assert(check(true).run(1, 1, SimpleRNG(42)) == Proved)
        assert(check(false).run(1, 1, SimpleRNG(42)) == Falsified("()", 0))
    }

    test("forAll") {
        val bool = Gen.boolean

        val sizedSGen = SGen.listOf(bool)
        val prop1 = forAll(sizedSGen)(list => list.forall(b => b || !b))
        val stream1 = new ByteArrayOutputStream()
        Console.withOut(stream1) {
            Prop.run(prop1, 3, 100, SimpleRNG(42))
            assert(stream1.toString contains "counterexample")
        }

        val unsizedSGen = Gen.listOfN(3, bool).unsized
        val prop2 = forAll(unsizedSGen)(list => list.forall(b => b || !b))
        val stream2 = new ByteArrayOutputStream()
        Console.withOut(stream2) {
            Prop.run(prop2, 3, 100, SimpleRNG(42))
            assert(stream2.toString contains "proved")
        }
    }
}
