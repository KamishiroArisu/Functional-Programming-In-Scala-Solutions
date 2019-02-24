package functional.programming.in.scala.chapter3

import org.scalatest.FunSuite
import functional.programming.in.scala.chapter3.Exercise5.List

class Exercise5Test extends FunSuite {
    test("dropWhile") {
        val list = List(1, 2, 3, 4, 5, 6, 7)
        assert(Exercise5.dropWhile(list, (x: Int) => x < 5) == List(5, 6, 7))
        assert(Exercise5.dropWhile(list, (x: Int) => x < 10) == List())
        assert(Exercise5.dropWhile(list, (x: Int) => x < 0) == List(1, 2, 3, 4, 5, 6, 7))
        assert(Exercise5.dropWhile(List(), (_: Any) => true) == List())
    }
}
