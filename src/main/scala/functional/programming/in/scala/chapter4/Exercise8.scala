package functional.programming.in.scala.chapter4

object Exercise8 {
    sealed trait AccumulativeEither[+E, +A] {
        def map[B](f: A => B): AccumulativeEither[E, B] = this match {
            case Left(e) => Left(e)
            case Right(a) => Right(f(a))
        }

        def map2[EE >: E,B,C](b: AccumulativeEither[EE, B])(f: (A, B) => C): AccumulativeEither[EE, C] = this match {
            case Left(e) => b match {
                case Left(eb) => Left(e ++ eb)
                case _ => Left(e)
            }
            case Right(a) => b.map(f(a, _))
        }
    }

    case class Left[+E](value: Seq[E]) extends AccumulativeEither[E, Nothing]
    case class Right[+A](value: A) extends AccumulativeEither[Nothing, A]
}
