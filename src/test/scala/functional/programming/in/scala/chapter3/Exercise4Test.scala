package functional.programming.in.scala.chapter3

import org.scalatest.FunSuite
import functional.programming.in.scala.chapter3.Exercise4.List

class Exercise4Test extends FunSuite {
    test("drop") {
        val list = List(1, 2, 3, 4, 5)
        assert(Exercise4.drop(list, 3) == List(4, 5))
        assert(Exercise4.drop(list, 0) == List(1, 2, 3, 4, 5))
        assert(Exercise4.drop(list, 6) == List())
    }
}
