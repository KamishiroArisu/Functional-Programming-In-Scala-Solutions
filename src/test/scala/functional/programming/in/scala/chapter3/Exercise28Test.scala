package functional.programming.in.scala.chapter3

import org.scalatest.FunSuite
import functional.programming.in.scala.chapter3.Exercise28._

class Exercise28Test extends FunSuite {
    test("map") {
        val root = Branch(Branch(Branch(Branch(Leaf(1), Branch(Leaf(2), Leaf(3))), Leaf(4)), Leaf(5)), Leaf(6))
        val expect = Branch(Branch(Branch(Branch(Leaf(1), Branch(Leaf(4), Leaf(9))), Leaf(16)), Leaf(25)), Leaf(36))
        assert(Exercise28.map(root)((x: Int) => x * x) == expect)
        assert(Exercise28.map(Leaf(3))((x: Int) => -x) == Leaf(-3))
    }
}
