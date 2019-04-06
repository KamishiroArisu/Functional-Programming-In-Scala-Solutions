package functional.programming.in.scala.chapter7

import java.util.concurrent.Executors

import org.scalatest.FunSuite
import functional.programming.in.scala.chapter7.Exercise6._

class Exercise6Test extends FunSuite {
    test("parFilter") {
        val es = Executors.newFixedThreadPool(10)
        assert(Exercise6.run(es)(parFilter(List(0, 1, 2, 3))(_ > 1)).get() == List(2, 3))
    }
}
