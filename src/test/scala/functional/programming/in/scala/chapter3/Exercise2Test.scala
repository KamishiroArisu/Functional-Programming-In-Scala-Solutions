package functional.programming.in.scala.chapter3

import org.scalatest.FunSuite
import functional.programming.in.scala.chapter3.Exercise2.List

class Exercise2Test extends FunSuite {
    test("tail") {
        val list = List(1, 2, 3, 4)
        val tail = Exercise2.tail(list)
        assert(tail == List(2, 3, 4))

        val empty = List()
        assert(Exercise2.tail(empty) == List())
    }
}
