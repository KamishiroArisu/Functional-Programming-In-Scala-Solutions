package functional.programming.in.scala.chapter6

import org.scalatest.FunSuite
import functional.programming.in.scala.chapter6.Exercise9._

class Exercise9Test extends FunSuite {
    test("map") {
        assert(map(int)(i => -i)(SimpleRNG(42))._1 == -16159453)
    }

    test("map2") {
        assert(map2(int, int)(_ + _)(SimpleRNG(42))._1 == (16159453 - 1281479697))
    }
}
