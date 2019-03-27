package functional.programming.in.scala.chapter6

object Exercise11 {
    sealed trait Input
    case object Coin extends Input
    case object Turn extends Input

    case class Machine(locked: Boolean, candies: Int, coins: Int)

    def simulateMachine(inputs: List[Input]): State[Machine, (Int, Int)] = for {
        _ <- State.sequence(inputs.map(i => State.modify(doInput(i))))
        m <- State.get
    } yield (m.coins, m.candies)

    def doInput(input: Input)(machine: Machine): Machine = (machine, input) match {
        case (Machine(true, x, y), Coin) if x > 0 => Machine(false, x, y + 1)
        case (Machine(false, x, y), Turn) => Machine(true, x - 1, y)
        case (m@Machine(_, _, _), _) => m
    }

    case class State[S,+A](run: S => (A, S)) {
        def flatMap[B](f: A => State[S,B]): State[S,B] = State(s => {
            val (a, s2) = run(s)
            f(a).run(s2)
        })

        def map[B](f: A => B): State[S,B] =
            flatMap(a => State.unit(f(a)))

        def map2[B,C](sb: State[S,B])(f: (A, B) => C): State[S,C] =
            flatMap(a => sb.map(b => f(a, b)))
    }

    object State {
        def unit[S,A](a: A): State[S,A] = State(s => (a, s))

        def sequence[S,A](fs: List[State[S,A]]): State[S,List[A]] =
            fs.foldLeft(unit[S,List[A]](List[A]()))((z, sa) => z.map2(sa)((l, a) => a :: l)).map(_.reverse)

        def get[S]: State[S,S] = State(s => (s, s))

        def set[S](s: S): State[S,Unit] = State(_ => ((), s))

        def modify[S](f: S => S): State[S,Unit] = for {
            s <- get
            _ <- set(f(s))
        } yield ()
    }
}
