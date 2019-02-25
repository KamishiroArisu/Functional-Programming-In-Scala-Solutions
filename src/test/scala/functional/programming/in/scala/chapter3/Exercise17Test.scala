package functional.programming.in.scala.chapter3

import org.scalatest.FunSuite
import functional.programming.in.scala.chapter3.Exercise17.List

class Exercise17Test extends FunSuite {
    test("convert") {
        assert(Exercise17.convert(List(1.0, 3.0, 7.7)) == List("1.0", "3.0", "7.7"))
        assert(Exercise17.convert(List()) == List())
    }
}
