package functional.programming.in.scala.chapter7

import java.util.concurrent.Executors

import org.scalatest.FunSuite
import functional.programming.in.scala.chapter7.Exercise14._

class Exercise14Test extends FunSuite {
    test("join") {
        val es = Executors.newFixedThreadPool(5)
        assert(Exercise14.run(es)(join(unit(unit(1)))).get == 1)
        assert(Exercise14.run(es)(join(lazyUnit(unit(2)))).get == 2)
        assert(Exercise14.run(es)(join(lazyUnit(lazyUnit(3)))).get == 3)
    }

    test("flatMap") {
        val es = Executors.newFixedThreadPool(5)
        assert(Exercise14.run(es)(flatMap(unit(1))(i => lazyUnit(i + 2))).get == 3)
        assert(Exercise14.run(es)(flatMap(lazyUnit("hello"))(i => lazyUnit(i + "world"))).get == "helloworld")
    }

    test("joinViaFlatMap") {
        val es = Executors.newFixedThreadPool(5)
        assert(Exercise14.run(es)(joinViaFlatMap(unit(unit(1)))).get == 1)
        assert(Exercise14.run(es)(joinViaFlatMap(lazyUnit(unit(2)))).get == 2)
        assert(Exercise14.run(es)(joinViaFlatMap(lazyUnit(lazyUnit(3)))).get == 3)
    }
}
