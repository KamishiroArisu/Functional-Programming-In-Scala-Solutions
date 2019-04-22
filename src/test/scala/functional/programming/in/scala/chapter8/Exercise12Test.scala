package functional.programming.in.scala.chapter8

import org.scalatest.FunSuite
import functional.programming.in.scala.chapter8.Exercise12._

class Exercise12Test extends FunSuite {
    test("listOf") {
        val sgen = SGen.listOf(Gen.union(Gen.unit(0), Gen.unit(1)))
        val list = sgen.forSize(1000).sample.run(SimpleRNG(42))._1
        assert(list.size == 1000)
        assert(450 until 550 contains list.sum)
    }
}
