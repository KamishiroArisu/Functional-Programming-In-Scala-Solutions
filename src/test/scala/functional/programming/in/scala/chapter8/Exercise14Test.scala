package functional.programming.in.scala.chapter8

import java.io.ByteArrayOutputStream

import org.scalatest.FunSuite
import functional.programming.in.scala.chapter8.Exercise14._

class Exercise14Test extends FunSuite {
    test("sorted") {
        val stream = new ByteArrayOutputStream()
        Console.withOut(stream) {
            Prop.run(Exercise14.sortedProp)
            assert(stream.toString contains "OK")
        }
    }
}
