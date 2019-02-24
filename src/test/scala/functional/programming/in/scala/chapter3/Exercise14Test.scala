package functional.programming.in.scala.chapter3

import org.scalatest.FunSuite
import functional.programming.in.scala.chapter3.Exercise14.List

class Exercise14Test extends FunSuite {
    test("appendViaFoldLeft") {
        assert(Exercise14.appendViaFoldLeft(List(1, 2, 3), List(4, 5)) == List(1, 2, 3, 4, 5))
        assert(Exercise14.appendViaFoldLeft(List(1, 2, 3), List()) == List(1, 2, 3))
        assert(Exercise14.appendViaFoldLeft(List(), List(4, 5)) == List(4, 5))
        assert(Exercise14.appendViaFoldLeft(List(), List()) == List())
    }

    test("appendViaFoldRight") {
        assert(Exercise14.appendViaFoldRight(List(1, 2, 3), List(4, 5)) == List(1, 2, 3, 4, 5))
        assert(Exercise14.appendViaFoldRight(List(1, 2, 3), List()) == List(1, 2, 3))
        assert(Exercise14.appendViaFoldRight(List(), List(4, 5)) == List(4, 5))
        assert(Exercise14.appendViaFoldRight(List(), List()) == List())
    }
}
