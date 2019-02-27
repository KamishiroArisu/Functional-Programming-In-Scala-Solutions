package functional.programming.in.scala.chapter3

object Exercise29 {
    def fold[A,B](t: Tree[A])(f: A => B)(g: (B, B) => B): B = t match {
        case Leaf(a) => f(a)
        case Branch(x, y) => g(fold(x)(f)(g), fold(y)(f)(g))
    }

    def size[A](t: Tree[A]): Int =
        fold(t)(_ => 1)((x, y) => x + y + 1)

    def maximum(t: Tree[Int]): Int =
        fold(t)((i: Int) => i)(_ max _)

    def depth[A](t: Tree[A]): Int =
        fold(t)(_ => 0)((x, y) => (x max y) + 1)

    def map[A,B](t: Tree[A])(f: A => B): Tree[B] =
        fold(t)(a => Leaf(f(a)): Tree[B])(Branch(_, _))

    sealed trait Tree[+A]
    case class Leaf[A](value: A) extends Tree[A]
    case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]
}
