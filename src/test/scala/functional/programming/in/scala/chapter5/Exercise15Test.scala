package functional.programming.in.scala.chapter5

import org.scalatest.FunSuite
import functional.programming.in.scala.chapter5.Exercise15._

class Exercise15Test extends FunSuite {
    test("tails") {
        assert(Stream(1, 2, 3).tails.map(s => s.toList).toList == List(List(1, 2, 3), List(2, 3), List(3), List()))
        assert(Stream().tails.map(s => s.toList).toList == List(List()))
    }
}
