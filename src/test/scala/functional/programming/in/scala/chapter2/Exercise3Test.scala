package functional.programming.in.scala.chapter2

import org.scalatest.FunSuite

class Exercise3Test extends FunSuite {
    test("curry") {
        val fun = (x: Int, y: Int) => x + y
        val curriedFun = Exercise3.curry(fun)
        val plus2 = curriedFun(2)
        val result = plus2(7)

        assert(result == 9)
    }
}
