package functional.programming.in.scala.chapter3

import org.scalatest.FunSuite
import functional.programming.in.scala.chapter3.Exercise21.List

class Exercise21Test extends FunSuite {
    test("filterViaFlatMap") {
        assert(Exercise21.filterViaFlatMap(List(1, 2, 3, 4, 5))(_ % 2 == 1) == List(1, 3, 5))
        assert(Exercise21.filterViaFlatMap(List())((x: Any) => true) == List())
    }
}
