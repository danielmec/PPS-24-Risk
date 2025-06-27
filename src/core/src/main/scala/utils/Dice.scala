package utils

import scala.util.Random

object Dice:
  def roll(): Int = 
    Random.nextInt(6) + 1
    
  def rollMany(n: Int): Seq[Int] =
    Seq.fill(n)(roll()).sorted(Ordering[Int].reverse)
