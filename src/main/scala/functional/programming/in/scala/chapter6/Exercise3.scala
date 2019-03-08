package functional.programming.in.scala.chapter6

object Exercise3 {
    def intDouble(rng: RNG): ((Int, Double), RNG) = {
        val (i, r1) = rng.nextInt
        val (d, r2) = double(r1)
        ((i, d), r2)
    }

    def doubleInt(rng: RNG): ((Double, Int), RNG) = intDouble(rng) match {
        case ((i, d), r) => ((d, i), r)
    }

    def double3(rng: RNG): ((Double, Double, Double), RNG) = {
        val (d1, r1) = double(rng)
        val (d2, r2) = double(r1)
        val (d3, r3) = double(r2)
        ((d1, d2, d3), r3)
    }

    def double(rng: RNG): (Double, RNG) = {
        val (i, r) = nonNegativeInt(rng)
        (i / (Int.MaxValue.toDouble + 1), r)
    }

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
