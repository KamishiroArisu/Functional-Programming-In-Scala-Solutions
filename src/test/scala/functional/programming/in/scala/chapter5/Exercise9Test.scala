package functional.programming.in.scala.chapter5

import org.scalatest.FunSuite
import functional.programming.in.scala.chapter5.Exercise9._

class Exercise9Test extends FunSuite {
    test("from") {
        assert(from(3).take(5).toList == (3 to 7).toList)
        assert(from(1).takeWhile(_ < 10).toList == (1 until 10).toList)
    }
}
