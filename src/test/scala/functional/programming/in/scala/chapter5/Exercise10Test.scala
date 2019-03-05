package functional.programming.in.scala.chapter5

import org.scalatest.FunSuite
import functional.programming.in.scala.chapter5.Exercise10._

class Exercise10Test extends FunSuite {
    test("fibs") {
        assert(fibs().take(7).toList == List(0, 1, 1, 2, 3, 5, 8))
    }
}
