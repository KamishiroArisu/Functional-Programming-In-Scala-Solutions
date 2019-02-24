package functional.programming.in.scala.chapter3

import org.scalatest.FunSuite
import functional.programming.in.scala.chapter3.Exercise11.List

class Exercise11Test extends FunSuite {
    test("sum") {
        assert(Exercise11.sum(List(1, 2, 3, 4, 5)) == 15)
        assert(Exercise11.sum(List()) == 0)
    }

    test("product") {
        assert(Exercise11.product(List(1.0, 2.0, 3.0, 4.0)) == 24.0)
        assert(Exercise11.product(List()) == 1.0)
    }

    test("length") {
        assert(Exercise11.length(List(1, 2, 3, 4, 5)) == 5)
        assert(Exercise11.length(List(1.0, 2.0, 3.0, 4.0)) == 4)
        assert(Exercise11.length(List()) == 0)
    }
}
