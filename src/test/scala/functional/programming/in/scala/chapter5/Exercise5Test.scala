package functional.programming.in.scala.chapter5

import org.scalatest.FunSuite
import functional.programming.in.scala.chapter5.Exercise5._

class Exercise5Test extends FunSuite {
    test("takeWhile") {
        assert(Stream(1, 2, 3, 4, 5, 6, 7).takeWhile(_ < 6).toList == List(1, 2, 3, 4, 5))
        assert(Stream(1, 2, 3, 4, 5, 6, 7).takeWhile(_ < 0).toList == List())
        assert(Stream().takeWhile(_ => true).toList == List())
    }
}
