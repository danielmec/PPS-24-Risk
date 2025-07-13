package utils

import scala.util.Random

/**
  * Utility object for rolling dice in the game.
  */
object Dice:
  /**
    * Rolls a single six-sided die.
    * @return The result of the roll (1 to 6).
    */
  def roll(): Int = 
    Random.nextInt(6) + 1
    
  /**
    * Rolls multiple dice and returns the results in descending order.
    * @param n The number of dice to roll.
    * @return A sequence of dice results, sorted descending.
    */
  def rollMany(n: Int): Seq[Int] =
    Seq.fill(n)(roll()).sorted(Ordering[Int].reverse)
