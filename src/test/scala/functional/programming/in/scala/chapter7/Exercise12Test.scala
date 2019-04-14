package functional.programming.in.scala.chapter7

import java.util.concurrent.Executors

import org.scalatest.FunSuite
import functional.programming.in.scala.chapter7.Exercise12._

class Exercise12Test extends FunSuite {
    test("choiceMap") {
        val es = Executors.newFixedThreadPool(5)
        assert(Exercise12.run(es)(choiceMap(lazyUnit("k1"))(Map("k1" -> unit("v1"), "k2" -> lazyUnit("v2")))).get == "v1")
        assert(Exercise12.run(es)(choiceMap(unit("k2"))(Map("k1" -> unit("v1"), "k2" -> lazyUnit("v2")))).get == "v2")
    }
}