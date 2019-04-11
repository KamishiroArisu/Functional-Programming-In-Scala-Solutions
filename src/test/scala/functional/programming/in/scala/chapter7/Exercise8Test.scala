package functional.programming.in.scala.chapter7

import java.util.concurrent.{TimeUnit, TimeoutException, Executors}

import org.scalatest.FunSuite
import functional.programming.in.scala.chapter7.Exercise8._

class Exercise8Test extends FunSuite {
    test("deadlock") {
        val es = Executors.newFixedThreadPool(1)
        assertThrows[TimeoutException](Exercise8.run(es)(fork(fork(unit(1)))).get(10, TimeUnit.SECONDS))
    }
}
