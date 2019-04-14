package functional.programming.in.scala.chapter8

import org.scalatest.FunSuite
import functional.programming.in.scala.chapter8.Exercise3._

class Exercise3Test extends FunSuite {
    test("&&") {
        val success = new Prop {
            override def check: Boolean = true
        }

        val failure = new Prop {
            override def check: Boolean = false
        }

        assert((success && success).check)
        assert(!(success && failure).check)
        assert(!(failure && success).check)
        assert(!(failure && failure).check)
    }
}
