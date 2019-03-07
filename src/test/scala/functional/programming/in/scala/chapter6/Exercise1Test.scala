package functional.programming.in.scala.chapter6

import org.scalatest.FunSuite
import functional.programming.in.scala.chapter6.Exercise1._

class Exercise1Test extends FunSuite {
    test("nonNegativeInt") {
        def loop(ra: RNG, n: Int): Boolean = {
            if (n < 0) {
                true
            }
            else {
                val (i, r) = nonNegativeInt(ra)
                assert(i >= 0)
                loop(r, n - 1)
            }
        }

        assert(loop(SimpleRNG(42), 500))
    }
}
