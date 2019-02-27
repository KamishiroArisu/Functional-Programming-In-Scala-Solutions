package functional.programming.in.scala.chapter3

object Exercise26 {
    def maximum(t: Tree[Int]): Int = t match {
        case Leaf(x) => x
        case Branch(x, y) => maximum(x) max maximum(y)
    }

    sealed trait Tree[+A]
    case class Leaf[A](value: A) extends Tree[A]
    case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]
}
