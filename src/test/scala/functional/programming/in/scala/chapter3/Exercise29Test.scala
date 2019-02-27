package functional.programming.in.scala.chapter3

import org.scalatest.FunSuite
import functional.programming.in.scala.chapter3.Exercise29._

class Exercise29Test extends FunSuite {
    test("size") {
        val root = Branch(Branch(Leaf(1), Leaf(2)), Leaf(3))
        assert(Exercise29.size(root) == 5)
        assert(Exercise29.size(Leaf(1)) == 1)
    }

    test("maximum") {
        val root = Branch(Branch(Leaf(-1), Leaf(0)), Leaf(42))
        assert(Exercise29.maximum(root) == 42)
        assert(Exercise29.maximum(Leaf(3)) == 3)
    }

    test("depth") {
        val root = Branch(Branch(Branch(Branch(Leaf(1), Branch(Leaf(2), Leaf(3))), Leaf(4)), Leaf(5)), Leaf(6))
        assert(Exercise29.depth(root) == 5)
        assert(Exercise29.depth(Leaf(1)) == 0)
    }

    test("map") {
        val root = Branch(Branch(Branch(Branch(Leaf(1), Branch(Leaf(2), Leaf(3))), Leaf(4)), Leaf(5)), Leaf(6))
        val expect = Branch(Branch(Branch(Branch(Leaf(1), Branch(Leaf(4), Leaf(9))), Leaf(16)), Leaf(25)), Leaf(36))
        assert(Exercise29.map(root)((x: Int) => x * x) == expect)
        assert(Exercise29.map(Leaf(3))((x: Int) => -x) == Leaf(-3))
    }
}
