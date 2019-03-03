package functional.programming.in.scala.chapter5

import org.scalatest.FunSuite
import functional.programming.in.scala.chapter5.Exercise3._

class Exercise3Test extends FunSuite {
    test("takeWhile") {
        assert(Stream(1, 2, 3, 4, 5, 6, 7).takeWhile(_ < 6).toList == List(1, 2, 3, 4, 5))
        assert(Stream().takeWhile(_ => true).toList == List())
    }
}
