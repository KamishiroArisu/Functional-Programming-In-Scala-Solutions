package functional.programming.in.scala.chapter8

import org.scalatest.FunSuite
import functional.programming.in.scala.chapter8.Exercise10._

class Exercise10Test extends FunSuite {
    test("unsized") {
        val sg = Gen.unit(3).unsized

        val g1 = sg.forSize(1)
        val g20 = sg.forSize(20)

        assert(g1.sample.run(SimpleRNG(42))._1 == g20.sample.run(SimpleRNG(42))._1)
    }
}
