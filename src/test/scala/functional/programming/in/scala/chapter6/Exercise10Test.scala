package functional.programming.in.scala.chapter6

import org.scalatest.FunSuite
import functional.programming.in.scala.chapter6.Exercise10._

class Exercise10Test extends FunSuite {
    test("unit") {
        assert(State.unit("hello").run(SimpleRNG(42))._1 == "hello")
    }

    test("map") {
        assert(State.unit("hello").map(s => s + "world").run(SimpleRNG(42))._1 == "helloworld")
    }

    test("map2") {
        assert(State.unit[RNG,String]("hello").map2(State.unit("world"))((s1, s2) => s1 + s2).run(SimpleRNG(42))._1
                == "helloworld")
    }

    test("flatMap") {
        assert(State.unit[RNG,String]("hello").flatMap(s => State.unit(s + "world")).run(SimpleRNG(42))._1
                == "helloworld")
    }

    test("sequence") {
        assert(State.sequence[RNG,String](List.fill(3)(State.unit("hello"))).run(SimpleRNG(42))._1
                == List("hello", "hello", "hello"))
    }
}
