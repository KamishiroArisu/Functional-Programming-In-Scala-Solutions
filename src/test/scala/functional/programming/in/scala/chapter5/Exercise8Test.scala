package functional.programming.in.scala.chapter5

import org.scalatest.FunSuite
import functional.programming.in.scala.chapter5.Exercise8._

class Exercise8Test extends FunSuite {
    test("constant") {
        assert(constant("3").take(5).toList == List.fill(5)("3"))
        assert(constant(7).filter(_ > 0).drop(3).take(1).toList == List(7))
    }
}
