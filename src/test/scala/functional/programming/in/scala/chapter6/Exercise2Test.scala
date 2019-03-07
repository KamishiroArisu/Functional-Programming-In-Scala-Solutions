package functional.programming.in.scala.chapter6

import org.scalatest.FunSuite
import functional.programming.in.scala.chapter6.Exercise2._

class Exercise2Test extends FunSuite {
    test("double") {
        def loop(ra: RNG, n: Int): Boolean = {
            if (n < 0) {
                true
            }
            else {
                val (i, r) = double(ra)
                assert(i >= 0.0 && i < 1.0)
                loop(r, n - 1)
            }
        }

        assert(loop(SimpleRNG(42), 500))
    }
}
