package functional.programming.in.scala.chapter3

import org.scalatest.FunSuite
import functional.programming.in.scala.chapter3.Exercise6.List

class Exercise6Test extends FunSuite {
    test("init") {
        val list = List(1, 2, 3, 4)
        assert(Exercise6.init(list) == List(1, 2, 3))
        assert(Exercise6.init(List(1)) == List())
        assert(Exercise6.init(List()) == List())
    }
}
