package functional.programming.in.scala.chapter4

import org.scalatest.FunSuite
import functional.programming.in.scala.chapter4.Exercise7._

class Exercise7Test extends FunSuite {
    test("traverse") {
        assert(traverse(List(1, 3, 5))((i: Int) => if (i % 2 == 1) Right(i) else Left("even")) == Right(List(1, 3, 5)))
        assert(traverse(List(1, 3, 4, 5, 6))((i: Int) => if (i % 2 == 1) Right(i) else Left("even" + i)) == Left("even4"))
        assert(traverse(List())((i: Int) => if (i % 2 == 1) Right(i) else Left("even")) == Right(List()))
    }

    test("sequence") {
        assert(sequence(List(Right(1))) == Right(List(1)))
        assert(sequence(List(Right(1), Right(2), Right(4))) == Right(List(1, 2, 4)))
        assert(sequence(List(Right(1), Right(2), Left("err1"), Right(4), Left("err2"))) == Left("err1"))
        assert(sequence(List()) == Right(List()))
    }
}
