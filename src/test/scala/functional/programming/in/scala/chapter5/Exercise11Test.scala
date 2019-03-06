package functional.programming.in.scala.chapter5

import org.scalatest.FunSuite
import functional.programming.in.scala.chapter5.Exercise11._

class Exercise11Test extends FunSuite {
    test("unfold") {
        assert(unfold(2)(i => if (i < 257) Some(i, i * i) else None).toList == List(2, 4, 16, 256))
        assert(unfold(0)(_ => None: Option[(Int, Int)]).toList == List())
    }
}
