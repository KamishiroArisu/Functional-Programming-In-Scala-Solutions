package functional.programming.in.scala.chapter8

import java.io.ByteArrayOutputStream

import org.scalatest.FunSuite
import functional.programming.in.scala.chapter8.Exercise13._

class Exercise13Test extends FunSuite {
    test("listOf1") {
        val smallInt = Gen.choose(-10, 10)
        val maxProp = forAll(SGen.listOf1(smallInt)) { ns =>
            val max = ns.max
            !ns.exists(_ > max)
        }
        val stream = new ByteArrayOutputStream()
        Console.withOut(stream) {
            Prop.run(maxProp)
            assert(stream.toString contains "OK")
        }
    }
}
