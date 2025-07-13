package model.cards

import model.player.PlayerColor
import model.board.Continent

/**
 * Represents a generic objective card in the game.
 * Each objective card provides a textual description of the mission to be completed by the player.
 */
sealed trait ObjectiveCard:
  /**
   * A textual description of the objective to display to the player.
   * 
   * @return a human-readable description of the objective
   */
  def description: String

/**
 * Companion object for the ObjectiveCard trait containing specific types of objective cards.
 */
object ObjectiveCard:

  /**
   * Objective requiring the player to conquer a specific number of territories,
   * each with a minimum number of troops.
   *
   * @param num               the number of territories to conquer
   * @param minTroopsToOwn    the minimum number of troops each conquered territory must have
   */
  case class ConquerTerritories(num: Int, minTroopsToOwn: Int = 1) extends ObjectiveCard:
    override def description: String =
      s"Conquer $num territories with at least $minTroopsToOwn troops each."

  /**
   * Objective requiring the player to conquer a specific set of continents.
   *
   * @param continents the set of continents the player must conquer
   */
  case class ConquerContinents(continents: Set[Continent]) extends ObjectiveCard:
    override def description: String =
      val continentNames = continents.map(_.name).mkString(", ")
      s"Conquer the following continents: $continentNames."

  /**
   * Objective requiring the player to conquer any N continents.
   *
   * @param n the number of continents to conquer (any of them)
   */
  case class ConquerNContinents(n: Int) extends ObjectiveCard:
    override def description: String =
      s"Conquer any $n continents"
