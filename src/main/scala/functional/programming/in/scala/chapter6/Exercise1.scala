package functional.programming.in.scala.chapter6

object Exercise1 {
    def nonNegativeInt(rng: RNG): (Int, RNG) = {
        val (i, r) = rng.nextInt
        if (i < 0)
            (-(i + 1), r)
        else
            (i, r)
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
