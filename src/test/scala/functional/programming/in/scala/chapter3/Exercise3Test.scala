package functional.programming.in.scala.chapter3

import org.scalatest.FunSuite
import functional.programming.in.scala.chapter3.Exercise3.List

class Exercise3Test extends FunSuite {
    test("setHead") {
        val list = List(1, 2, 3, 4, 5)
        val modified = Exercise3.setHead(list, 8080)

        assert(modified == List(8080, 2, 3, 4, 5))
        assert(list == List(1, 2, 3, 4, 5))
    }
}
