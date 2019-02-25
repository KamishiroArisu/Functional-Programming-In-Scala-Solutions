package functional.programming.in.scala.chapter3

import org.scalatest.FunSuite
import functional.programming.in.scala.chapter3.Exercise18.List

class Exercise18Test extends FunSuite {
    test("map") {
        assert(Exercise18.map(List(1, 2, 3, 4))((x: Int) => x + 1) == List(2, 3, 4, 5))
        assert(Exercise18.map(List())((x: Int) => x + 1) == List())

        assert(Exercise18.map(List(1.2, 2.5, 0.0))(_.toString) == List("1.2", "2.5", "0.0"))
        assert(Exercise18.map(List())(_.toString) == List())
    }
}
