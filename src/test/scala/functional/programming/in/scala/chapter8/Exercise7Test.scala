package functional.programming.in.scala.chapter8

import org.scalatest.FunSuite
import functional.programming.in.scala.chapter8.Exercise7._

class Exercise7Test extends FunSuite {
    test("union") {
        val g1 = Gen.unit(0)
        val g2 = Gen.unit(1)
        val list = Gen.union(g1, g2).listOfN(Gen.unit(1000)).sample.run(SimpleRNG(42))._1
        assert(list.forall(e => e == 0 || e == 1))
        assert(450 until 550 contains list.sum)
    }
}
