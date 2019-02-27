package functional.programming.in.scala.chapter3

import org.scalatest.FunSuite
import functional.programming.in.scala.chapter3.Exercise26._

class Exercise26Test extends FunSuite {
    test("maximum") {
        val root = Branch(Branch(Leaf(-1), Leaf(0)), Leaf(42))
        assert(Exercise26.maximum(root) == 42)
        assert(Exercise26.maximum(Leaf(3)) == 3)
    }
}
