package functional.programming.in.scala.chapter5

object Exercise6 {
    sealed trait Stream[+A] {
        def toList: List[A] = this match {
            case Cons(h, t) => h() :: t().toList
            case Empty => List()
        }

        def take(n: Int): Stream[A] = this match {
            case Cons(h, t) if n > 0 => Stream.cons(h(), t().take(n - 1))
            case _ => Stream.empty
        }

        def drop(n: Int): Stream[A] = this match {
            case Cons(h, t) if n > 0 => t().drop(n - 1)
            case _ => this
        }

        def foldRight[B](z: => B)(f: (A, => B) => B): B = this match {
            case Cons(h, t) => f(h(), t().foldRight(z)(f))
            case _ => z
        }

        def forAll(p: A => Boolean): Boolean =
            foldRight(true)((a, b) => p(a) && b)

        def takeWhile(p: A => Boolean): Stream[A] =
            foldRight(Stream.empty[A])((a, b) => if (p(a)) Stream.cons(a, b) else Stream.empty)

        def headOption: Option[A] =
            foldRight(None: Option[A])((a, _) => Some(a))
    }

    case object Empty extends Stream[Nothing]
    case class Cons[+A](h: () => A, t: () => Stream[A]) extends Stream[A]

    object Stream {
        def cons[A](hd: => A, tl: => Stream[A]): Stream[A] = {
            lazy val head = hd
            lazy val tail = tl
            Cons(() => head, () => tail)
        }

        def empty[A]: Stream[A] = Empty

        def apply[A](as: A*): Stream[A] =
            if (as.isEmpty)
                empty
            else
                cons(as.head, apply(as.tail: _*))
    }
}
