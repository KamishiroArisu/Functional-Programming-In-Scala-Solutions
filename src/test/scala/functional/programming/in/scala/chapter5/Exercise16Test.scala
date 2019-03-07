package functional.programming.in.scala.chapter5

import org.scalatest.FunSuite
import functional.programming.in.scala.chapter5.Exercise16._

class Exercise16Test extends FunSuite {
    test("scanRight") {
        assert(Stream(1, 2, 3).scanRight(0)(_ + _).toList == List(6, 5, 3, 0))
        assert(Stream("1", "2", "3", "4").scanRight("")(_ + _).toList == List("1234", "234", "34", "4", ""))
        assert(Stream.empty[Int].scanRight(3)(_ + _).toList == List(3))
    }
}
