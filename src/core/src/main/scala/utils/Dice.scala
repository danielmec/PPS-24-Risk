package utils

import scala.util.Random

object Dice:
  def roll(n: Int): Seq[Int] =
    Seq.fill(n)(Random.nextInt(6) + 1).sorted(Ordering[Int].reverse)
