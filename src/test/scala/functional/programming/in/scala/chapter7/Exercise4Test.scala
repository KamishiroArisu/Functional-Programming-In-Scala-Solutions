package functional.programming.in.scala.chapter7

import java.util.concurrent.Executors

import org.scalatest.FunSuite
import functional.programming.in.scala.chapter7.Exercise4._

class Exercise4Test extends FunSuite {
    test("asyncF") {
        val es = Executors.newFixedThreadPool(10)
        assert(Exercise4.run(es)(asyncF((i: Int) => i * i)(13)).get() == 169)
        assert(Exercise4.run(es)(asyncF((s: String) => s + Thread.currentThread().getName)("hello")).get().startsWith("hellopool"))
    }
}
