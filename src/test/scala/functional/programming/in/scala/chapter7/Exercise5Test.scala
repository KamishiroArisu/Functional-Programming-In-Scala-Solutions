package functional.programming.in.scala.chapter7

import java.util.concurrent.Executors

import org.scalatest.FunSuite
import functional.programming.in.scala.chapter7.Exercise5._

class Exercise5Test extends FunSuite {
    test("sequence") {
        val es = Executors.newFixedThreadPool(10)
        assert(Exercise5.run(es)(sequence(List(unit(1), unit(2), unit(3)))).get() == List(1, 2, 3))
    }
}
