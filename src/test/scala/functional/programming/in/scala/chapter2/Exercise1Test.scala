package functional.programming.in.scala.chapter2

import org.scalatest.FunSuite

class Exercise1Test extends FunSuite {
    test("fibonacci") {
        assert(Exercise1.fibonacci(3) == 1)
        assert(Exercise1.fibonacci(5) == 3)
        assert(Exercise1.fibonacci(9) == 21)
    }
}
