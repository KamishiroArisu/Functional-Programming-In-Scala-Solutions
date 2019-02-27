package functional.programming.in.scala.chapter3

object Exercise27 {
    def depth[A](t: Tree[A]): Int = t match {
        case Leaf(_) => 0
        case Branch(x, y) => 1 + (depth(x) max depth(y))
    }

    sealed trait Tree[+A]
    case class Leaf[A](value: A) extends Tree[A]
    case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]
}
