package functional.programming.in.scala.chapter7

import java.util.concurrent.Executors

import org.scalatest.FunSuite
import functional.programming.in.scala.chapter7.Exercise10._

class Exercise10Test extends FunSuite {
    test("failure") {
        val es = Executors.newFixedThreadPool(10)
        assert(Exercise10.run(es)(unit(1)) == Right(1))
        assert(Exercise10.run(es)(fork(unit(2))) == Right(2))

        val e = Exercise10.run(es)(fork(unit(throw new RuntimeException("failure"))))
        assert(e.isLeft)
        assert(e.left.get.getMessage == "failure")
    }
}
