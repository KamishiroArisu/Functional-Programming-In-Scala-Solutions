package functional.programming.in.scala.chapter3

import org.scalatest.FunSuite
import functional.programming.in.scala.chapter3.Exercise23.List

class Exercise23Test extends FunSuite {
    test("zipWith") {
        assert(Exercise23.zipWith(List(1, 2, 3), List("4", "5", "6"))((i, s) => i + s.toInt) == List(5, 7, 9))
        assert(Exercise23.zipWith(List(1, 2, 3), List())((i, s: String) => i + s.toInt) == List())
    }
}
