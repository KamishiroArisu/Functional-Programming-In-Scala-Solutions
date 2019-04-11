package functional.programming.in.scala.chapter7

import java.util.concurrent.{TimeUnit, TimeoutException, Executors}

import org.scalatest.FunSuite
import functional.programming.in.scala.chapter7.Exercise9._

class Exercise9Test extends FunSuite {
    test("deadlock") {
        val es2 = Executors.newFixedThreadPool(2)
        assertThrows[TimeoutException](Exercise9.run(es2)(fork(fork(fork(unit(1))))).get(10, TimeUnit.SECONDS))

        val es5 = Executors.newFixedThreadPool(5)
        assertThrows[TimeoutException](Exercise9.run(es5)(fork(fork(fork(fork(fork(fork(unit(1)))))))).get(10, TimeUnit.SECONDS))
    }
}
