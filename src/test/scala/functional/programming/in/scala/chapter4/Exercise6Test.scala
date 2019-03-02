package functional.programming.in.scala.chapter4

import org.scalatest.FunSuite
import functional.programming.in.scala.chapter4.Exercise6._

class Exercise6Test extends FunSuite {
    test("map") {
        assert(Right(6).map(_ + 1) == Right(7))
        assert(Left("error").map((i: Int) => i + 1) == Left("error"))
    }

    test("flatMap") {
        assert(Right(6).flatMap(i => if (i % 2 == 0) Right(i) else Left("odd")) == Right(6))
        assert(Right(7).flatMap(i => if (i % 2 == 0) Right(i) else Left("odd")) == Left("odd"))
        assert(Left("err1").flatMap((i: Int) => if (i % 2 == 0) Right(i) else Left("err2")) == Left("err1"))
    }

    test("orElse") {
        assert(Right(6).orElse(Right(12)) == Right(6))
        assert(Left("error").orElse(Right(12)) == Right(12))
        assert(Left("error1").orElse(Left("error2")) == Left("error2"))
    }

    test("map2") {
        assert(Right(6).map2(Right(8))(_ + _) == Right(14))
        assert(Right(6).map2(Left("err"))((x: Int, y: Int) => x + y) == Left("err"))
        assert(Left("err1").map2(Left("err1"))((x: Int, y: Int) => x + y) == Left("err1"))
    }
}
