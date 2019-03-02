package functional.programming.in.scala.chapter4

object Exercise7 {
    def sequence[E, A](es: List[Either[E, A]]): Either[E, List[A]] =
        traverse(es)(x => x)

    def traverse[E,A,B](as: List[A])(f: A => Either[E, B]): Either[E, List[B]] =
        as.foldRight[Either[E, List[B]]](Right(List()))((a, el) => f(a).flatMap(b => el.map(b :: _)))

    sealed trait Either[+E, +A] {
        def map[B](f: A => B): Either[E, B] = this match {
            case Left(e) => Left(e)
            case Right(a) => Right(f(a))
        }

        def flatMap[EE >: E,B](f: A => Either[EE, B]): Either[EE, B] = this match {
            case Left(e) => Left(e)
            case Right(a) => f(a)
        }

        def orElse[EE >: E,B >: A](b: => Either[EE, B]): Either[EE, B] = this match {
            case Left(e) => b
            case Right(a) => Right(a)
        }

        def map2[EE >: E,B,C](b: Either[EE, B])(f: (A, B) => C): Either[EE, C] = this match {
            case Left(e) => Left(e)
            case Right(a) => b.map(f(a, _))
        }
    }

    case class Left[+E](value: E) extends Either[E, Nothing]
    case class Right[+A](value: A) extends Either[Nothing, A]
}
