package functional.programming.in.scala.chapter6

import org.scalatest.FunSuite
import functional.programming.in.scala.chapter6.Exercise8._

class Exercise8Test extends FunSuite {
    test("flatMap") {
        assert(flatMap(int)(i => unit(-i))(SimpleRNG(42))._1 == -16159453)
    }

    test("nonNegativeLessThan") {
        val a = nonNegativeLessThan(100)(SimpleRNG(42))._1
        assert(0 <= a && a < 100)
    }
}
