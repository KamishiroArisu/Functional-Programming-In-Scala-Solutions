package functional.programming.in.scala.chapter8

import java.io.ByteArrayOutputStream

import org.scalatest.FunSuite
import functional.programming.in.scala.chapter8.Exercise16._

class Exercise16Test extends FunSuite {
    test("pint2") {
        val p = pint2
        val p1 = forAllPar(p)(pi => map(pi)(i => 0 until 10 contains i))
        val p2 = forAllPar(p)(n => equal(map(n)(y => y), n))

        val stream1 = new ByteArrayOutputStream()
        Console.withOut(stream1) {
            Prop.run(p1, 3, 10, SimpleRNG(42))
            assert(stream1.toString contains "OK")
        }

        val stream2 = new ByteArrayOutputStream()
        Console.withOut(stream2) {
            Prop.run(p2, 3, 10, SimpleRNG(42))
            assert(stream2.toString contains "OK")
        }
    }
}
