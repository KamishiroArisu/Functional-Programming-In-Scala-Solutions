package functional.programming.in.scala.chapter2

import org.scalatest.FunSuite

class Exercise5Test extends FunSuite {
    test("compose") {
        val plus1 = (x: Int) => x + 1
        val square = (x: Int) => x * x
        val composedFun = Exercise5.compose(square, plus1)
        val result = composedFun(3)

        assert(result == 16)
    }
}
