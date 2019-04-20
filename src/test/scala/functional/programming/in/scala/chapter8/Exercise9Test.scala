package functional.programming.in.scala.chapter8

import org.scalatest.FunSuite
import functional.programming.in.scala.chapter8.Exercise9._

class Exercise9Test extends FunSuite {
    test("&&") {
        val p1 = forAll(Gen.choose(1, 10))(_ > 0)
        val p2 = forAll(Gen.unit(2))(_ == 2)
        val p3 = forAll(Gen.choose(0, 10))(_ > 20)
        val p4 = forAll(Gen.unit(3))(_ == 0)
        val p5 = forAll(Gen.choose(0, 1))(10 / _ == 10)

        val pass = (p1 && p2).run(10, SimpleRNG(42))
        assert(!pass.isFalsified)

        val fail1 = (p1 && p4).run(10, SimpleRNG(42))
        assert(fail1.isFalsified)
        assert(fail1.asInstanceOf[Falsified].successes == 10)
        assert(fail1.asInstanceOf[Falsified].failure == "3")

        val fail2 = (p4 && p3).run(10, SimpleRNG(42))
        assert(fail2.isFalsified)
        assert(fail2.asInstanceOf[Falsified].successes == 0)
        assert(fail2.asInstanceOf[Falsified].failure == "3")

        val fail3 = (p1 && p5).run(10, SimpleRNG(42))
        assert(fail3.isFalsified)
        assert(fail3.asInstanceOf[Falsified].successes == 10)
        assert(fail3.asInstanceOf[Falsified].failure.contains("/ by zero"))
    }

    test("||") {
        val p1 = forAll(Gen.choose(1, 10))(_ > 0)
        val p2 = forAll(Gen.unit(2))(_ == 2)
        val p3 = forAll(Gen.unit(0))(_ > 20)
        val p4 = forAll(Gen.unit(3))(_ == 0)
        val p5 = forAll(Gen.choose(0, 1))(10 / _ == 10)

        val pass1 = (p1 || p2).run(10, SimpleRNG(42))
        assert(!pass1.isFalsified)

        val pass2 = (p1 || p3).run(10, SimpleRNG(42))
        assert(!pass2.isFalsified)

        val pass3 = (p1 || p5).run(10, SimpleRNG(42))
        assert(!pass3.isFalsified)

        val pass4 = (p4 || p1).run(10, SimpleRNG(42))
        assert(!pass4.isFalsified)

        val fail1 = (p3 || p4).run(10, SimpleRNG(42))
        assert(fail1.isFalsified)
        assert(fail1.asInstanceOf[Falsified].successes == 0)
        assert(fail1.asInstanceOf[Falsified].failure.contains("0"))
        assert(fail1.asInstanceOf[Falsified].failure.contains("3"))

        val fail2 = (p3 || p5).run(10, SimpleRNG(42))
        assert(fail2.isFalsified)
        assert(fail2.asInstanceOf[Falsified].successes == 0)
        assert(fail1.asInstanceOf[Falsified].failure.contains("0"))
        assert(fail2.asInstanceOf[Falsified].failure.contains("/ by zero"))
    }
}
