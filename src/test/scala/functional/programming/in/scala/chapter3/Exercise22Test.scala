package functional.programming.in.scala.chapter3

import org.scalatest.FunSuite
import functional.programming.in.scala.chapter3.Exercise22.List

class Exercise22Test extends FunSuite {
    test("listAdd") {
        assert(Exercise22.listAdd(List(1, 2, 3), List(4, 6, 8, 9)) == List(5, 8, 11))
        assert(Exercise22.listAdd(List(), List(1, 3, 7)) == List())
    }
}
