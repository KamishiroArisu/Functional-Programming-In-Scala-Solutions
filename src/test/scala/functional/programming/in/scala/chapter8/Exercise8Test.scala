package functional.programming.in.scala.chapter8

import functional.programming.in.scala.chapter8.Exercise8._
import org.scalatest.FunSuite

class Exercise8Test extends FunSuite {
     test("weighted") {
         val g1 = (Gen.unit(0), 0.8)
         val g2 = (Gen.unit(1), 0.2)
         val list = Gen.weighted(g1, g2).listOfN(Gen.unit(1000)).sample.run(SimpleRNG(42))._1
         assert(list.forall(e => e == 0 || e == 1))
         assert(150 until 250 contains list.sum)
     }
 }
