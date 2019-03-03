package functional.programming.in.scala.chapter5

import org.scalatest.FunSuite
import functional.programming.in.scala.chapter5.Exercise2._

class Exercise2Test extends FunSuite {
    test("take") {
        assert(Stream(1, 2, 3, 4, 5).take(2).toList == Stream(1, 2).toList)
        assert(Stream(1, 2, 3, 4, 5).take(7).toList == Stream(1, 2, 3, 4, 5).toList)
        assert(Stream().take(2).toList == Stream().toList)
    }

    test("drop") {
        assert(Stream(1, 2, 3, 4, 5, 6, 7).drop(4).toList == Stream(5, 6, 7).toList)
        assert(Stream(1, 2, 3, 4).drop(7).toList == Stream().toList)
        assert(Stream().drop(3).toList == Stream().toList)
    }
}
