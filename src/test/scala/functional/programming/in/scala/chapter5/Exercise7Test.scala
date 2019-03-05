package functional.programming.in.scala.chapter5

import org.scalatest.FunSuite
import functional.programming.in.scala.chapter5.Exercise7._

class Exercise7Test extends FunSuite {
    test("map") {
        assert(Stream(1, 2, 3).map(_.toString).toList == List("1", "2", "3"))
        assert(Stream(1, 2, 3, 4).map(_ + 1).toList == List(2, 3, 4, 5))
        assert(Stream().map(x => x).toList == List())
    }

    test("filter") {
        assert(Stream(1, 2, 3, 4, 5).filter(_ % 2 == 0).toList == List(2, 4))
        assert(Stream(1, 3, 5, 7, 9).filter(_ % 2 == 0).toList == List())
        assert(Stream().filter(_ => true).toList == List())
    }

    test("append") {
        assert(Stream(1, 2, 3).append(Stream(4, 5)).toList == List(1, 2, 3, 4, 5))
        assert(Stream().append(Stream(1, 2, 3, 4, 5)).toList == List(1, 2, 3, 4, 5))
        assert(Stream(1, 2, 3, 4, 5).append(Stream()).toList == List(1, 2, 3, 4, 5))
    }

    test("flatMap") {
        assert(Stream(1, 2, 3).flatMap((i: Int) => Stream(i, i + 1)).toList == List(1, 2, 2, 3, 3, 4))
        assert(Stream().flatMap((i: Int) => Stream(i, i + 1)).toList == List())
        assert(Stream(1, 2, 3).flatMap((i: Int) => Stream()).toList == List())
    }
}
