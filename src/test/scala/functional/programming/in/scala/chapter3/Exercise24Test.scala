package functional.programming.in.scala.chapter3

import org.scalatest.FunSuite
import functional.programming.in.scala.chapter3.Exercise24.List

class Exercise24Test extends FunSuite {
    test("hasSubsequence") {
        assert(Exercise24.hasSubsequence(List(1, 2, 3, 4), List(1, 2)))
        assert(Exercise24.hasSubsequence(List(1, 2, 3, 4), List(2, 3)))
        assert(Exercise24.hasSubsequence(List(1, 2, 3, 4), List(4)))
        assert(Exercise24.hasSubsequence(List(1, 2, 3, 4), List()))
        assert(Exercise24.hasSubsequence(List(), List()))
    }
}
