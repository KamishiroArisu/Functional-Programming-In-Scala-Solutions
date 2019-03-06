package functional.programming.in.scala.chapter5

import org.scalatest.FunSuite
import functional.programming.in.scala.chapter5.Exercise13._

class Exercise13Test extends FunSuite {
    test("map") {
        assert(Stream(1, 2, 3).map(_.toString).toList == List("1", "2", "3"))
        assert(Stream(1, 2, 3, 4).map(_ + 1).toList == List(2, 3, 4, 5))
        assert(Stream().map(x => x).toList == List())
    }

    test("take") {
        assert(Stream(1, 2, 3, 4, 5).take(2).toList == Stream(1, 2).toList)
        assert(Stream(1, 2, 3, 4, 5).take(7).toList == Stream(1, 2, 3, 4, 5).toList)
        assert(Stream().take(2).toList == Stream().toList)
    }

    test("takeWhile") {
        assert(Stream(1, 2, 3, 4, 5, 6, 7).takeWhile(_ < 6).toList == List(1, 2, 3, 4, 5))
        assert(Stream().takeWhile(_ => true).toList == List())
    }

    test("zipWith") {
        assert(Stream(1, 2, 3, 4).zipWith(Stream(2, 3, 4))((x: Int, y: Int) => x + y).toList == List(3, 5, 7))
        assert(Stream().zipWith(Stream(2, 3, 4))((x: Int, y: Int) => x + y).toList == List())
        assert(Stream(1, 2, 3, 4).zipWith(Stream())((x: Int, y: Int) => x + y).toList == List())
        assert(Stream().zipWith(Stream())((x: Int, y: Int) => x + y).toList == List())
    }

    test("zipAll") {
        assert(Stream(1, 2, 3, 4).zipAll(Stream(2, 3, 4)).toList == List((Some(1), Some(2)), (Some(2), Some(3)),
            (Some(3), Some(4)), (Some(4), None)))
        assert(Stream(1, 2, 3).zipAll(Stream(2, 3, 4, 5)).toList == List((Some(1), Some(2)), (Some(2), Some(3)),
            (Some(3), Some(4)), (None, Some(5))))
        assert(Stream(1, 2).zipAll(Stream()).toList == List((Some(1), None), (Some(2), None)))
        assert(Stream().zipAll(Stream(1, 2)).toList == List((None, Some(1)), (None, Some(2))))
    }
}
