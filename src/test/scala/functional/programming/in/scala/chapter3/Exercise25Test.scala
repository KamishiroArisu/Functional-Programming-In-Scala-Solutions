package functional.programming.in.scala.chapter3

import org.scalatest.FunSuite
import functional.programming.in.scala.chapter3.Exercise25._

class Exercise25Test extends FunSuite {
    test("size") {
        val root = Branch(Branch(Leaf(1), Leaf(2)), Leaf(3))
        assert(Exercise25.size(root) == 5)
        assert(Exercise25.size(Leaf(1)) == 1)
    }
}
