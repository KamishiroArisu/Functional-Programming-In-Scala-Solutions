package functional.programming.in.scala.chapter3

import org.scalatest.FunSuite
import functional.programming.in.scala.chapter3.Exercise12.List

class Exercise12Test extends FunSuite {
    test("reverse") {
        assert(Exercise12.reverse(List(1, 2, 3)) == List(3, 2, 1))
        assert(Exercise12.reverse(List("a", "ab", "abc", "d")) == List("d", "abc", "ab", "a"))
        assert(Exercise12.reverse(List()) == List())
    }
}
