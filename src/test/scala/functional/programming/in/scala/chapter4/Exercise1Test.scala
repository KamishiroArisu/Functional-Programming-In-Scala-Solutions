package functional.programming.in.scala.chapter4

import org.scalatest.FunSuite
import functional.programming.in.scala.chapter4.Exercise1._

class Exercise1Test extends FunSuite {
    test("map") {
        val a: Option[Int] = Some(18)
        val b: Option[Int] = None
        assert(a.map(x => x + 2) == Some(20))
        assert(b.map(x => x + 3) == None)
    }

    test("flatMap") {
        val a: Option[Int] = Some(18)
        val b: Option[Int] = Some(17)
        val c: Option[Int] = None
        assert(a.flatMap(x => if (x % 2 == 0) Some(x + 1) else None) == Some(19))
        assert(b.flatMap(x => if (x % 2 == 0) Some(x + 1) else None) == None)
        assert(c.flatMap(x => Some(x)) == None)
    }

    test("getOrElse") {
        val a: Option[Int] = Some(18)
        val b: Option[Int] = None
        assert(a.getOrElse(-1) == 18)
        assert(b.getOrElse(-1) == -1)
    }

    test("orElse") {
        val a: Option[Int] = Some(18)
        val b: Option[Int] = None
        assert(a.orElse(Some(3)) == Some(18))
        assert(b.orElse(Some(3)) == Some(3))
    }

    test("filter") {
        val a: Option[Int] = Some(18)
        val b: Option[Int] = Some(17)
        val c: Option[Int] = None
        assert(a.filter(x => x % 2 == 0) == Some(18))
        assert(b.filter(x => x % 2 == 0) == None)
        assert(c.filter(_ => true) == None)
    }
}
