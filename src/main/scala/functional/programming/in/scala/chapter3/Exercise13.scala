package functional.programming.in.scala.chapter3

object Exercise13 {
    def foldLeftViaFoldRight[A,B](as: List[A], z: B)(f: (B, A) => B): B =
        List.foldRight(as, (b: B) => b)((a: A, g: B => B) => (b: B) => g(f(b, a)))(z)

    def foldRightViaFoldLeft[A,B](as: List[A], z: B)(f: (A, B) => B): B =
        List.foldLeft(as, (b: B) => b)((g: B => B, a: A) => (b: B) => g(f(a, b)))(z)

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

        def apply[A](as: A*): List[A] =
            if (as.isEmpty)
                Nil
            else
                Cons(as.head, apply(as.tail: _*))
    }
}
