package functional.programming.in.scala.chapter2

object Exercise2 {
    def isSorted[A](arr: Array[A], ordered: (A, A) => Boolean): Boolean = {
        @annotation.tailrec
        def loop(arr: Array[A], ordered: (A, A) => Boolean, i: Int): Boolean = {
            if (i >= arr.length)
                true
            else if (!ordered(arr(i -1), arr(i)))
                false
            else
                loop(arr, ordered, i + 1)
        }

        if (arr.length < 2)
            true
        else
            loop(arr, ordered, 1)
    }
}
