package functional.programming.in.scala.chapter3

import org.scalatest.FunSuite
import functional.programming.in.scala.chapter3.Exercise16.List

class Exercise16Test extends FunSuite {
    test("plusOne") {
        assert(Exercise16.plusOne(List(1, 2, 3, 4)) == List(2, 3, 4, 5))
        assert(Exercise16.plusOne(List()) == List())
    }
}
