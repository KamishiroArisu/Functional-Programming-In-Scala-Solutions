package functional.programming.in.scala.chapter4

import org.scalatest.FunSuite
import functional.programming.in.scala.chapter4.Exercise2._

class Exercise2Test extends FunSuite {
    test("variance") {
        assert(variance(List(1.0, 2.0, 3.0, 4.0)).getOrElse(0.0) == 1.25)
        assert(variance(List()).getOrElse(18.0) == 18.0)
    }
}
