package functional.programming.in.scala.chapter5

import org.scalatest.FunSuite
import functional.programming.in.scala.chapter5.Exercise6._

class Exercise6Test extends FunSuite {
    test("headOption") {
        assert(Stream(1, 2, 3).headOption.contains(1))
        assert(Stream().headOption.isEmpty)
    }
}
