package functional.programming.in.scala.chapter4

import org.scalatest.FunSuite
import functional.programming.in.scala.chapter4.Exercise4._

class Exercise4Test extends FunSuite {
    test("sequence") {
        assert(sequence(List(Some(1))) == Some(List(1)))
        assert(sequence(List(Some(1), Some(2), Some(4))) == Some(List(1, 2, 4)))
        assert(sequence(List(Some(1), Some(2), None, Some(4))) == None)
        assert(sequence(List()) == Some(List()))
    }
}
