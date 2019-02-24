package functional.programming.in.scala.chapter3

import org.scalatest.FunSuite
import functional.programming.in.scala.chapter3.Exercise10.List

class Exercise10Test extends FunSuite {
    test("foldLeft") {
        assert(Exercise10.foldLeft(List(1, 2, 3, 4, 5), 0)(_ + _) == 15)
        assert(Exercise10.foldLeft(List("a", "b", "c"), "z")(_ + _) == "zabc")
    }
}
