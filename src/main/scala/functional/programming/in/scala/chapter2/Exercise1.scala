package functional.programming.in.scala.chapter2

object Exercise1 {
    def fibonacci(n: Int): Int = {
        require(n > 0)

        @annotation.tailrec
        def loop(i: Int, j: Int, m: Int, n: Int): Int = {
            if (m == n)
                i
            else
                loop(j, i + j, m + 1, n)
        }

        loop(0, 1, 1, n)
    }
}
