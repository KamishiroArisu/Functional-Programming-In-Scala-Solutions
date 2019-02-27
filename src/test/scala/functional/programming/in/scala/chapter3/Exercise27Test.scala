package functional.programming.in.scala.chapter3

import org.scalatest.FunSuite
import functional.programming.in.scala.chapter3.Exercise27._

class Exercise27Test extends FunSuite {
    test("depth") {
        val root = Branch(Branch(Branch(Branch(Leaf(1), Branch(Leaf(2), Leaf(3))), Leaf(4)), Leaf(5)), Leaf(6))
        assert(Exercise27.depth(root) == 5)
        assert(Exercise27.depth(Leaf(1)) == 0)
    }
}
