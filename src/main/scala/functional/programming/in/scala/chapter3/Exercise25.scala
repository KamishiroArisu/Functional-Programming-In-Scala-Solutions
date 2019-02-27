package functional.programming.in.scala.chapter3

object Exercise25 {
    def size[A](t: Tree[A]): Int = t match {
        case Leaf(_) => 1
        case Branch(l, r) => 1 + size(l) + size(r)
    }

    sealed trait Tree[+A]
    case class Leaf[A](value: A) extends Tree[A]
    case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]
}
