package functional.programming.in.scala.chapter2

import org.scalatest.FunSuite

class Exercise2Test extends FunSuite {
    test("isSorted") {
        val sortedIntArray = (1 until 10).toArray
        assert(Exercise2.isSorted(sortedIntArray, (x: Int, y: Int) => x < y))

        val unsordedStringArray = Array("a", "ab", "abcd", "abc")
        assert(!Exercise2.isSorted(unsordedStringArray, (x: String, y: String) => x.length < y.length))

        val singleIntArray = Array(8080)
        assert(Exercise2.isSorted(singleIntArray, (_: Int, _: Int) => false))
    }
}
