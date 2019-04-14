package functional.programming.in.scala.chapter8

import org.scalatest.FunSuite
import functional.programming.in.scala.chapter8.Exercise4._

class Exercise4Test extends FunSuite {
    test("choose") {
        val r = SimpleRNG(42)
        val (i1, r1) = Gen.choose(1, 10).sample.run(r)
        val (i2, r2) = Gen.choose(0, 100).sample.run(r1)
        val (i3, _) = Gen.choose(-1, 10).sample.run(r2)

        assert(1 until 10 contains i1)
        assert(0 until 100 contains i2)
        assert(-1 until 10 contains i3)
    }
}
