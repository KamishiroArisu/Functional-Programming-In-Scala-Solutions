package functional.programming.in.scala.chapter3

import org.scalatest.FunSuite
import functional.programming.in.scala.chapter3.Exercise15.List

class Exercise15Test extends FunSuite {
    test("flatten") {
        val list = List(List(1, 2, 3), List(4, 5), List(), List(6))
        assert(Exercise15.flatten(list) == List(1, 2, 3, 4, 5, 6))
    }
}
