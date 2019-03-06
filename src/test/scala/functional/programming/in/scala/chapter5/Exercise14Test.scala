package functional.programming.in.scala.chapter5

import org.scalatest.FunSuite
import functional.programming.in.scala.chapter5.Exercise14._

class Exercise14Test extends FunSuite {
    test("startsWith") {
        assert(Stream(1, 2, 3, 4).startsWith(Stream(1, 2, 3)))
        assert(!Stream(1, 2, 3, 4).startsWith(Stream(1, 3, 4)))
        assert(!Stream(1, 2, 3, 4).startsWith(Stream(1, 2, 3, 4, 5)))
        assert(Stream(1, 2, 3, 4).startsWith(Stream()))
        assert(Stream(1, 2, 3, 4).startsWith(Stream(1)))
    }
}
