package functional.programming.in.scala.chapter7

import java.util.concurrent.Executors

import org.scalatest.FunSuite
import functional.programming.in.scala.chapter7.Exercise2._

class Exercise2Test extends FunSuite {
    test("unit") {
        assert(Exercise2.run(Executors.newFixedThreadPool(3))(unit(Thread.currentThread().getName)).get().startsWith("ScalaTest"))
    }

    test("map2") {
        assert(Exercise2.run(Executors.newFixedThreadPool(3))(map2(unit(1), unit(2))(_ + _)).get() == 3)
    }

    test("fork") {
        assert(Exercise2.run(Executors.newFixedThreadPool(3))(fork(unit(Thread.currentThread().getName))).get().startsWith("pool"))
    }

    test("lazyUnit") {
        assert(Exercise2.run(Executors.newFixedThreadPool(3))(lazyUnit(Thread.currentThread().getName)).get().startsWith("pool"))
    }

    test("run") {
        assert(Exercise2.run(Executors.newFixedThreadPool(3))(fork(unit(Thread.currentThread().getName))).get().startsWith("pool"))
    }
}
