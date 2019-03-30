package functional.programming.in.scala.chapter7

import java.util.concurrent.{TimeUnit, Callable, ExecutorService, Future}


object Exercise2 {
    type Par[A] = ExecutorService => Future[A]

    def unit[A](a: A): Par[A] = _ => UnitFuture(a)

    def map2[A,B,C](a: Par[A], b: Par[B])(f: (A, B) => C): Par[C] =
        es => {
            val fa = a(es)
            val fb = b(es)
            UnitFuture(f(fa.get, fb.get))
        }

    def fork[A](a: => Par[A]): Par[A] = es =>
        es.submit(new Callable[A] {
            override def call(): A = a(es).get
        })


    def lazyUnit[A](a: => A): Par[A] = fork(unit(a))

    def run[A](es: ExecutorService)(a: Par[A]): Future[A] = a(es)

    private case class UnitFuture[A](get: A) extends Future[A] {
        override def isCancelled: Boolean = false

        override def get(timeout: Long, unit: TimeUnit): A = get

        override def cancel(mayInterruptIfRunning: Boolean): Boolean = false

        override def isDone: Boolean = true
    }
}
