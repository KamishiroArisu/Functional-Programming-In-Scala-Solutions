package functional.programming.in.scala.chapter3

import org.scalatest.FunSuite
import functional.programming.in.scala.chapter3.Exercise20.List

class Exercise20Test extends FunSuite {
    test("flatMap") {
        assert(Exercise20.flatMap(List(1, 2, 5))(i => List(i, i + 1)) == List(1, 2, 2, 3, 5, 6))
        assert(Exercise20.flatMap(List())(i => List(i, i)) == List())
    }
}
