package functional.programming.in.scala.chapter6

import org.scalatest.FunSuite
import functional.programming.in.scala.chapter6.Exercise3._

class Exercise3Test extends FunSuite {
    test("intDouble") {
        val rng = SimpleRNG(42)
        val ((i, d), _) = intDouble(rng)
        assert(i.isInstanceOf[Int])
        assert(d.isInstanceOf[Double])
        assert(0 <= d && d < 1)
    }

    test("doubleInt") {
        val rng = SimpleRNG(42)
        val ((d, i), _) = doubleInt(rng)
        assert(i.isInstanceOf[Int])
        assert(d.isInstanceOf[Double])
        assert(0 <= d && d < 1)
    }

    test("double3") {
        val rng = SimpleRNG(42)
        val ((d1, d2, d3), _) = double3(rng)
        assert(d1.isInstanceOf[Double])
        assert(0 <= d1 && d1 < 1)
        assert(d2.isInstanceOf[Double])
        assert(0 <= d2 && d2 < 1)
        assert(d3.isInstanceOf[Double])
        assert(0 <= d3 && d3 < 1)
    }
}
