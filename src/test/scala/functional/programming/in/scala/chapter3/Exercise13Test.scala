package functional.programming.in.scala.chapter3

import org.scalatest.FunSuite
import functional.programming.in.scala.chapter3.Exercise13.List

class Exercise13Test extends FunSuite {
    test("foldLeftViaFoldRight") {
        val list = List("a", "b", "c", "d")
        assert(Exercise13.foldLeftViaFoldRight(list, "z")(_ + _) == "zabcd")
    }

    test("foldRightViaFoldLeft") {
        val list = List("a", "b", "c", "d")
        assert(Exercise13.foldRightViaFoldLeft(list, "z")(_ + _) == "abcdz")
    }
}
