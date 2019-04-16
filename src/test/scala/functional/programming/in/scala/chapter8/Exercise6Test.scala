package functional.programming.in.scala.chapter8

import org.scalatest.FunSuite
import functional.programming.in.scala.chapter8.Exercise6._

class Exercise6Test extends FunSuite {
    test("flatMap") {
        assert(Gen.unit(4).flatMap(i => Gen.unit(i * i)).sample.run(SimpleRNG(42))._1 == 16)
        assert(21 until 30 contains Gen.choose(1, 10).flatMap(i => Gen.unit(i + 20)).sample.run(SimpleRNG(42))._1)
    }

    test("listOfN") {
        val list = Gen.unit(8).listOfN(Gen.unit(5)).sample.run(SimpleRNG(42))._1
        assert(list.length == 5)
        assert(list.forall(_ == 8))

        val list2 = Gen.choose(1, 4).listOfN(Gen.choose(5, 8)).sample.run(SimpleRNG(42))._1
        assert(5 until 8 contains list2.length)
        assert(list2.forall(e => 1 until 4 contains e))
    }
}
