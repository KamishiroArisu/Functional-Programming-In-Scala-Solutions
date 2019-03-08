package functional.programming.in.scala.chapter6

import org.scalatest.FunSuite
import functional.programming.in.scala.chapter6.Exercise7._

class Exercise7Test extends FunSuite {
    test("sequence") {
        assert(sequence(List.fill(3)(int))(SimpleRNG(42))._1 == List(16159453, -1281479697, -340305902))
    }

    test("ints") {
        assert(ints(3)(SimpleRNG(42))._1 == List(16159453, -1281479697, -340305902))
    }
}
