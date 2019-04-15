package functional.programming.in.scala.chapter8

import org.scalatest.FunSuite
import functional.programming.in.scala.chapter8.Exercise5._

class Exercise5Test extends FunSuite {
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

    test("listOfN") {
        val r = SimpleRNG(42)
        val g = Gen.listOfN(10, Gen.unit(43))
        val (list, _) = g.sample.run(r)
        assert(list.length == 10)
        assert(list.forall(_ == 43))
    }
}
