package functional.programming.in.scala.chapter8

import java.io.ByteArrayOutputStream

import org.scalatest.FunSuite
import functional.programming.in.scala.chapter8.Exercise17._

class Exercise17Test extends FunSuite {
    test("fork") {
        val pint = Gen.choose(0, 10).map(unit)
        val p1 = forkProp(pint)

        val stream1 = new ByteArrayOutputStream()
        Console.withOut(stream1) {
            Prop.run(p1, 3, 10, SimpleRNG(42))
            assert(stream1.toString contains "OK")
        }
    }
}
