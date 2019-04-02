package functional.programming.in.scala.chapter7

import java.util.concurrent.{TimeoutException, TimeUnit, Executors}

import org.scalatest.FunSuite
import functional.programming.in.scala.chapter7.Exercise3._

class Exercise3Test extends FunSuite {
    test("get") {
        val fiveSeconds = lazyUnit({Thread.sleep(5000);12})
        val twoSeconds = lazyUnit({Thread.sleep(2000);25})
        val es = Executors.newFixedThreadPool(10)

        assert(Exercise3.run(es)(map2(fiveSeconds, twoSeconds)(_ + _)).get() == 37)
        assert(Exercise3.run(es)(map2(fiveSeconds, twoSeconds)(_ + _)).get(6, TimeUnit.SECONDS) == 37)
        assertThrows[TimeoutException](Exercise3.run(es)(map2(fiveSeconds, twoSeconds)(_ + _)).get(3, TimeUnit.SECONDS))

        val finish = Exercise3.run(es)(map2(fiveSeconds, twoSeconds)(_ + _))
        Thread.sleep(5000)
        assert(finish.get(1, TimeUnit.SECONDS) == 37)
    }

    test("isCancelled") {
        val fiveSeconds = lazyUnit({Thread.sleep(5000);12})
        val twoSeconds = lazyUnit({Thread.sleep(2000);25})
        val es = Executors.newFixedThreadPool(10)
        val cancel = Exercise3.run(es)(map2(fiveSeconds, twoSeconds)(_ + _))
        Thread.sleep(1000)
        cancel.cancel(true)
        assert(cancel.isCancelled)
    }

    test("isDone") {
        val fiveSeconds = lazyUnit({Thread.sleep(5000);12})
        val twoSeconds = lazyUnit({Thread.sleep(2000);25})
        val es = Executors.newFixedThreadPool(10)
        val done = Exercise3.run(es)(map2(fiveSeconds, twoSeconds)(_ + _))
        Thread.sleep(6000)
        assert(done.isDone)
    }
}
