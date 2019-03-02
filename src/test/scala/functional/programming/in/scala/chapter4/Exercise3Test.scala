package functional.programming.in.scala.chapter4

import org.scalatest.FunSuite
import functional.programming.in.scala.chapter4.Exercise3._

class Exercise3Test extends FunSuite {
    test("map2") {
        val f = (i: Int, s: String) => i.toString + s

        assert(map2(Some(1), Some("abc"))(f) == Some("1abc"))
        assert(map2(None: Option[Int], Some("abc"))(f) == None)
        assert(map2(Some(1), None: Option[String])(f) == None)
        assert(map2(None: Option[Int], None: Option[String])(f) == None)
    }
}
