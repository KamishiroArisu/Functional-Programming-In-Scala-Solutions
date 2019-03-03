package functional.programming.in.scala.chapter5

import org.scalatest.FunSuite
import functional.programming.in.scala.chapter5.Exercise1._

class Exercise1Test extends FunSuite {
    test("toList") {
        assert(Stream(1, 2, 3, 4, 5).toList == List(1, 2, 3, 4, 5))
        assert(Stream().toList == List())
    }
}
