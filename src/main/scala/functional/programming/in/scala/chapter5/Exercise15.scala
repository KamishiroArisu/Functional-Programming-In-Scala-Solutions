package functional.programming.in.scala.chapter5

object Exercise15 {
    sealed trait Stream[+A] {
        def toList: List[A] = this match {
            case Cons(h, t) => h() :: t().toList
            case Empty => List()
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

        def headOption: Option[A] =
            foldRight(None: Option[A])((a, _) => Some(a))

        def filter(f: A => Boolean): Stream[A] =
            foldRight(Stream.empty[A])((a, b) => if (f(a)) Stream.cons(a, b) else b)

        def append[B >: A](s: => Stream[B]): Stream[B] =
            foldRight(s)((a, b) => Stream.cons(a, b))

        def flatMap[B](f: A => Stream[B]): Stream[B] =
            foldRight(Stream.empty[B])((a, b) => f(a) append b)

        def map[B](f: A => B): Stream[B] =
            Stream.unfold(this) {
                case Cons(h, t) => Some((f(h()), t()))
                case _ => None
            }

        def take(n: Int): Stream[A] =
            Stream.unfold((this, n)) {
                case (Cons(h, t), n) if n > 0 => Some((h(), (t(), n - 1)))
                case _ => None
            }

        def takeWhile(p: A => Boolean): Stream[A] =
            Stream.unfold(this) {
                case Cons(h, t) if p(h()) => Some((h(), t()))
                case _ => None
            }

        def zipWith[B,C](bs: Stream[B])(f: (A, B) => C): Stream[C] =
            Stream.unfold((this, bs)) {
                case (Empty, _) => None
                case (_, Empty) => None
                case (Cons(ah, at), Cons(bh, bt)) => Some((f(ah(), bh()), (at(), bt())))
            }

        def zipAll[B](s2: Stream[B]): Stream[(Option[A], Option[B])] =
            Stream.unfold((this, s2)) {
                case (Empty, Empty) => None
                case (Empty, Cons(h, t)) => Some((None, Some(h())), (Empty, t()))
                case (Cons(h, t), Empty) => Some((Some(h()), None), (t(), Empty))
                case (Cons(ah, at), Cons(bh, bt)) => Some((Some(ah()), Some(bh())), (at(), bt()))
            }

        def startsWith[B >: A](s: Stream[B]): Boolean =
            zipAll(s).map {
                case (Some(x), Some(y)) => x == y
                case (_, None) => true
                case _ => false
            }.forAll(x => x)

        def tails: Stream[Stream[A]] =
            Stream.unfold(this) {
                case s@Cons(h, t) => Some((s, t()))
                case Empty => None
            }.append(Stream(Stream()))
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

        def unfold[A,S](z: S)(f: S => Option[(A, S)]): Stream[A] = {
            f(z) match {
                case Some((a, s)) => Stream.cons(a, unfold(s)(f))
                case _ => Stream.empty[A]
            }
        }
    }
}
