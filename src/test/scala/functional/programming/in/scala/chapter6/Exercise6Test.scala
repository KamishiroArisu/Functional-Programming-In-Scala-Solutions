package functional.programming.in.scala.chapter6

import org.scalatest.FunSuite
import functional.programming.in.scala.chapter6.Exercise6._

class Exercise6Test extends FunSuite {
    test("map2") {
        assert(map2(int, int)(_ + _)(SimpleRNG(42))._1 == (16159453 - 1281479697))
    }
}
