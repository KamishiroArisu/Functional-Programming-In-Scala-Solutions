package functional.programming.in.scala.chapter5

import org.scalatest.FunSuite
import functional.programming.in.scala.chapter5.Exercise12._

class Exercise12Test extends FunSuite {
    test("fibs") {
        assert(fibs().take(7).toList == List(0, 1, 1, 2, 3, 5, 8))
    }

    test("from") {
        assert(from(3).take(5).toList == (3 to 7).toList)
        assert(from(1).takeWhile(_ < 10).toList == (1 until 10).toList)
    }

    test("constant") {
        assert(constant("3").take(5).toList == List.fill(5)("3"))
        assert(constant(7).filter(_ > 0).drop(3).take(1).toList == List(7))
    }

    test("ones") {
        assert(ones.take(5).toList == List.fill(5)(1))
    }
}
