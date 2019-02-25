package functional.programming.in.scala.chapter3

object Exercise19 {
    def filter[A](as: List[A])(f: A => Boolean): List[A] =
        List.foldRight(as, Nil: List[A])((a, la) => if (f(a)) Cons(a, la) else la)

    sealed trait List[+A]
    case object Nil extends List[Nothing]
    case class Cons[+A](head: A, tail: List[A]) extends List[A]

    object List {
        def sum(ints: List[Int]): Int = ints match {
            case Nil => 0
            case Cons(x, xs) => x + sum(xs)
        }

        def product(ds: List[Double]): Double = ds match {
            case Nil => 1.0
            case Cons(0.0, _) => 0.0
            case Cons(x, xs) => x * product(xs)
        }

        def tail[A](l: List[A]): List[A] = l match {
            case Nil => Nil
            case Cons(x, xs) => xs
        }

        def foldRight[A,B](as: List[A], z: B)(f: (A, B) => B): B = as match {
            case Nil => z
            case Cons(x, xs) => f(x, foldRight(xs, z)(f))
        }

        def foldLeft[A,B](as: List[A], z: B)(f: (B, A) => B): B = as match {
            case Nil => z
            case Cons(x, xs) => foldLeft(xs, f(z, x))(f)
        }

        def append[A](a1: List[A], a2: List[A]): List[A] =
            List.foldRight(a1, a2)((a, b) => Cons(a, b))

        def map[A,B](as: List[A])(f: A => B): List[B] =
            List.foldRight(as, Nil: List[B])((a, lb) => Cons(f(a), lb))

        def apply[A](as: A*): List[A] =
            if (as.isEmpty)
                Nil
            else
                Cons(as.head, apply(as.tail: _*))
    }
}
