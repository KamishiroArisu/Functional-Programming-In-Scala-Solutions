package functional.programming.in.scala.chapter3

import org.scalatest.FunSuite
import functional.programming.in.scala.chapter3.Exercise8.List

class Exercise8Test extends FunSuite {
    test("result") {
        assert(Exercise8.result == List(1, 2, 3, 4))
    }
}
