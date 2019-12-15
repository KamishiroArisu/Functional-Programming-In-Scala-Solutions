package functional.programming.in.scala.chapter8

import java.io.ByteArrayOutputStream

import org.scalatest.FunSuite
import functional.programming.in.scala.chapter8.Exercise18._

class Exercise18Test extends FunSuite {
    test("takeWhileProp") {
        val g1 = Gen.boolean
        val prop1 = takeWhileProp(g1)(identity)

        val stream1 = new ByteArrayOutputStream()
        Console.withOut(stream1) {
            Prop.run(prop1, rng = SimpleRNG(42))
            assert(stream1.toString contains "OK, passed")
        }

        val g2 = Gen.choose(0, 10)
        val p2 = (i: Int) => i < 5
        val prop2 = takeWhileProp(g2)(p2)

        val stream2 = new ByteArrayOutputStream()
        Console.withOut(stream2) {
            Prop.run(prop2, rng = SimpleRNG(42))
            assert(stream2.toString contains "OK, passed")
        }
    }
}
