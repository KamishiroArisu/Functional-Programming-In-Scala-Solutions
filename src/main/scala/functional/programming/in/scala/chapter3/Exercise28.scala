package functional.programming.in.scala.chapter3

object Exercise28 {
    def map[A,B](t: Tree[A])(f: A => B): Tree[B] = t match {
        case Leaf(x) => Leaf(f(x))
        case Branch(x, y) => Branch(map(x)(f), map(y)(f))
    }

    sealed trait Tree[+A]
    case class Leaf[A](value: A) extends Tree[A]
    case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]
}
