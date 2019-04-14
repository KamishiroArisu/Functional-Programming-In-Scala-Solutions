package functional.programming.in.scala.chapter7

import java.util.concurrent.Executors

import org.scalatest.FunSuite
import functional.programming.in.scala.chapter7.Exercise11._

class Exercise11Test extends FunSuite {
    test("choiceN") {
        val es = Executors.newFixedThreadPool(5)
        assert(Exercise11.run(es)(choiceN(lazyUnit(2))(List(unit(1), unit(2), unit(3)))).get == 3)
        assert(Exercise11.run(es)(choiceN(unit(0))(List(lazyUnit(1), unit(2), unit(3)))).get == 1)
    }

    test("choice") {
        val es = Executors.newFixedThreadPool(5)
        assert(Exercise11.run(es)(choice(unit(true))(lazyUnit(3), unit(0))).get == 3)
        assert(Exercise11.run(es)(choice(lazyUnit(false))(lazyUnit(3), unit(0))).get == 0)
    }
}