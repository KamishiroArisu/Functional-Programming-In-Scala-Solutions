package functional.programming.in.scala.chapter6

object Exercise4 {
    def ints(count: Int)(rng: RNG): (List[Int], RNG) = {
        def loop(co: Int, rn: RNG): (List[Int], RNG) = {
            if (co > 0) {
                val (i, r2) = rn.nextInt
                val (li, r3) = loop(co - 1, r2)
                (i :: li, r3)
            } else {
                (Nil, rn)
            }
        }

        loop(count, rng)
    }


    sealed trait RNG {
        def nextInt: (Int, RNG)
    }

    case class SimpleRNG(seed: Long) extends RNG {
        override def nextInt: (Int, RNG) = {
            val newSeed = (seed * 0x5DEECE66DL + 0xBL) & 0xFFFFFFFFFFFFL
            val nextRNG = SimpleRNG(newSeed)
            val n = (newSeed >>> 16).toInt

            (n, nextRNG)
        }
    }
}
