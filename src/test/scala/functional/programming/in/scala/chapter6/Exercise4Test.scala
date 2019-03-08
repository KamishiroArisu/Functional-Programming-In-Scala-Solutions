package functional.programming.in.scala.chapter6

import org.scalatest.FunSuite
import functional.programming.in.scala.chapter6.Exercise4._

class Exercise4Test extends FunSuite {
    test("ints") {
        assert(ints(2)(SimpleRNG(42))._1 == List(16159453, -1281479697))
        assert(ints(0)(SimpleRNG(42))._1 == List())
    }
}
