package functional.programming.in.scala.chapter6

import org.scalatest.FunSuite
import functional.programming.in.scala.chapter6.Exercise11._

class Exercise11Test extends FunSuite {
    test("simulateMachine") {
        assert(simulateMachine(List(Coin, Turn, Coin, Turn, Turn)).run(Machine(true, 5, 2))._1 == (4, 3))
        assert(simulateMachine(List(Turn, Turn, Coin, Turn, Turn, Turn, Turn)).run(Machine(true, 5, 2))._1 == (3, 4))
        assert(simulateMachine(List(Turn, Turn, Turn, Turn, Turn, Turn)).run(Machine(true, 5, 2))._1 == (2, 5))
        assert(simulateMachine(List(Coin, Coin, Coin, Turn)).run(Machine(true, 5, 2))._1 == (3, 4))
    }
}
