package functional.programming.in.scala.chapter2

import org.scalatest.FunSuite

class Exercise4Test extends FunSuite{
    test("uncurry") {
        val curriedFun = (x: Int) => (y: Int) => x + y
        val uncurriedFun = Exercise4.uncurry(curriedFun)
        val result = uncurriedFun(2, 7)

        assert(result == 9)
    }
}
