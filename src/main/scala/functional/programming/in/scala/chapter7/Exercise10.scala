package functional.programming.in.scala.chapter7

import java.util.concurrent.{Callable, CountDownLatch, ExecutorService}
import java.util.concurrent.atomic.AtomicReference

object Exercise10 {
    sealed trait Future[A] {
        private[chapter7] def apply(s: A => Unit, f: Throwable => Unit): Unit
    }

    type Par[A] = ExecutorService => Future[A]

    def run[A](es: ExecutorService)(p: Par[A]): Either[Throwable, A] = {
        val ref = new AtomicReference[Either[Throwable, A]]
        val latch = new CountDownLatch(1)
        p(es)(a => {ref.set(Right(a)); latch.countDown()}, t => {ref.set(Left(t)); latch.countDown()})
        latch.await()
        ref.get
    }

    def unit[A](a: A): Par[A] =
        es => new Future[A] {
            def apply(s: (A) => Unit, f: (Throwable) => Unit): Unit =
                s(a)
        }

    def fork[A](a: => Par[A]): Par[A] =
        es => new Future[A] {
            def apply(s: (A) => Unit, f: (Throwable) => Unit): Unit =
                eval(es)(a)(s, f)
        }

    def eval[A](es: ExecutorService)(a: => Par[A])(s: (A) => Unit, f: (Throwable) => Unit) =
        es.submit(new Callable[Unit] {
            override def call(): Unit =
                try {
                    a(es)(s, f)
                } catch {
                    case t: Throwable => f(t)
                }
        })
}
