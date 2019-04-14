package functional.programming.in.scala.chapter7

import java.util.concurrent.atomic.AtomicReference
import java.util.concurrent.{TimeUnit, Callable, Future, ExecutorService}

object Exercise12 {
    type Par[A] = ExecutorService => Future[A]

    def unit[A](a: A): Par[A] = _ => UnitFuture(a)

    def map[A,B](pa: Par[A])(f: A => B): Par[B] =
        map2(pa, unit(()))((a, _) => f(a))

    def map2[A,B,C](a: Par[A], b: Par[B])(f: (A, B) => C): Par[C] =
        es => {
            val fa = a(es)
            val fb = b(es)
            Map2Future(fa, fb, f)
        }

    def fork[A](a: => Par[A]): Par[A] = es =>
        es.submit(new Callable[A] {
            override def call(): A = a(es).get
        })


    def lazyUnit[A](a: => A): Par[A] = fork(unit(a))

    def run[A](es: ExecutorService)(a: Par[A]): Future[A] = a(es)

    def asyncF[A,B](f: A => B): A => Par[B] =
        a => lazyUnit(f(a))

    def sequence[A](ps: List[Par[A]]): Par[List[A]] =
        ps.foldRight(unit(List[A]()))((pa, z) => map2(pa, z)(_ :: _))

    def parMap[A,B](ps: List[A])(f: A => B): Par[List[B]] = fork {
        val fbs = ps.map(asyncF(f))
        sequence(fbs)
    }

    def parFilter[A](as: List[A])(f: A => Boolean): Par[List[A]] =
        map(parMap(as)(a => if (f(a)) List(a) else List()))(_.flatten)

    def choiceN[A](n: Par[Int])(choices: List[Par[A]]): Par[A] =
        es => {
            val i = run(es)(n).get
            run(es)(choices(i))
        }

    def choice[A](cond: Par[Boolean])(t: Par[A], f: Par[A]): Par[A] =
        choiceN(map(cond)(if (_) 0 else 1))(List(t, f))

    def choiceMap[K,V](key: Par[K])(choices: Map[K,Par[V]]): Par[V] =
        es => {
            val k = run(es)(key).get
            run(es)(choices(k))
        }

    private case class UnitFuture[A](get: A) extends Future[A] {
        override def isCancelled: Boolean = false

        override def get(timeout: Long, unit: TimeUnit): A = get

        override def cancel(mayInterruptIfRunning: Boolean): Boolean = false

        override def isDone: Boolean = true
    }

    private case class Map2Future[A,B,C](f1: Future[A], f2: Future[B], f: (A, B) => C) extends Future[C] {
        val ref = new AtomicReference[Option[C]](None)

        override def isCancelled: Boolean =
            f1.isCancelled || f2.isCancelled

        override def get(): C =
            evaluate(Long.MaxValue)

        override def get(timeout: Long, unit: TimeUnit): C =
            evaluate(TimeUnit.MILLISECONDS.convert(timeout, unit))

        override def cancel(mayInterruptIfRunning: Boolean): Boolean =
            f1.cancel(mayInterruptIfRunning) || f2.cancel(mayInterruptIfRunning)

        override def isDone: Boolean =
            ref.get().isDefined || (f1.isDone && f2.isDone)

        private def evaluate(timeout: Long): C = {
            val t1 = System.currentTimeMillis()
            val v1 = f1.get(timeout, TimeUnit.MILLISECONDS)
            val t2 = System.currentTimeMillis()
            val remain = timeout - (t2 - t1)
            val v2 = f2.get(remain, TimeUnit.MILLISECONDS)
            val result = f(v1, v2)
            ref.set(Some(result))
            result
        }
    }
}
