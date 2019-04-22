package functional.programming.in.scala.chapter8

import org.scalatest.FunSuite
import functional.programming.in.scala.chapter8.Exercise11._

class Exercise11Test extends FunSuite {
    test("map") {
        val gen = Gen.unit(24)
        val sgen = gen.unsized.map(_ + 12)
        assert(sgen.forSize(10).sample.run(SimpleRNG(42))._1 == 36)
    }

    test("flatMap") {
        val gen = Gen.unit(24)
        val sgen = gen.unsized
        assert(sgen.flatMap(i => Gen.unit(i + 12).unsized).forSize(10).sample.run(SimpleRNG(42))._1 == 36)
    }
}
