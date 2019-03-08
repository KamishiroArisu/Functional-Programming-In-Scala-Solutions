package functional.programming.in.scala.chapter6

import org.scalatest.FunSuite
import functional.programming.in.scala.chapter6.Exercise5._

class Exercise5Test extends FunSuite {
    test("double") {
        val d = double(SimpleRNG(42))._1
        assert(d >= 0)
        assert(d < 1)
    }
}
