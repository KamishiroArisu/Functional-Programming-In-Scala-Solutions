package functional.programming.in.scala.chapter3

import org.scalatest.FunSuite
import functional.programming.in.scala.chapter3.Exercise9.List

class Exercise9Test extends FunSuite {
    test("length") {
        assert(Exercise9.length(List(1, 2, 3)) == 3)
        assert(Exercise9.length(List(1, 2, 3, 5, 9, 11)) == 6)
        assert(Exercise9.length(List()) == 0)
    }
}
