package functional.programming.in.scala.chapter3

import org.scalatest.FunSuite
import functional.programming.in.scala.chapter3.Exercise19.List

class Exercise19Test extends FunSuite {
    test("filter") {
        assert(Exercise19.filter(List(1, 2, 3, 4, 5, 6, 7))(_ % 2 == 1) == List(1, 3, 5, 7))
        assert(Exercise19.filter(List())((x: Any) => true) == List())
    }
}
