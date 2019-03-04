package functional.programming.in.scala.chapter5

import org.scalatest.FunSuite
import functional.programming.in.scala.chapter5.Exercise4._

class Exercise4Test extends FunSuite {
    test("forAll") {
        assert(Stream(0 until 9: _*).forAll(_ < 10))
        assert(!Stream(0 until 10000: _*).forAll(_ < 10))
        assert(Stream().forAll(_ => false))
    }
}
