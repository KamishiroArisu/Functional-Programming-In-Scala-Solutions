package functional.programming.in.scala.chapter4

import org.scalatest.FunSuite
import functional.programming.in.scala.chapter4.Exercise8._

class Exercise8Test extends FunSuite {
    test("map2") {
        assert(Left(Seq("err1")).map2(Left(Seq("err2")))((x: Int, y: Int) => x + y) == Left(Seq("err1", "err2")))
        assert(Left(Seq("err")).map2(Right(1))((x: Int, y: Int) => x + y) == Left(Seq("err")))
        assert(Right(1).map2(Left(Seq("err")))((x: Int, y: Int) => x + y) == Left(Seq("err")))
        assert(Right(1).map2(Right(3))((x: Int, y: Int) => x + y) == Right(4))
    }
}
