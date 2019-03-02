package functional.programming.in.scala.chapter4

import org.scalatest.FunSuite
import functional.programming.in.scala.chapter4.Exercise5._

class Exercise5Test extends FunSuite {
    test("traverse") {
        assert(traverse(List(1, 3, 5))((i: Int) => if (i % 2 == 1) Some(i) else None) == Some(List(1, 3, 5)))
        assert(traverse(List(1, 3, 4))((i: Int) => if (i % 2 == 1) Some(i) else None) == None)
        assert(traverse(List())((i: Int) => if (i % 2 == 1) Some(i) else None) == Some(List()))
    }

    test("sequenceViaTraverse") {
        assert(sequenceViaTraverse(List(Some(1))) == Some(List(1)))
        assert(sequenceViaTraverse(List(Some(1), Some(2), Some(4))) == Some(List(1, 2, 4)))
        assert(sequenceViaTraverse(List(Some(1), Some(2), None, Some(4))) == None)
        assert(sequenceViaTraverse(List()) == Some(List()))
    }
}
