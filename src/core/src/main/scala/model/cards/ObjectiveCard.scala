package model.cards

import model.player.PlayerColor
import model.board.Continent

sealed trait ObjectiveCard extends Card:
  def description: String

object ObjectiveCard:

  case class ConquerTerritories(num: Int, minTroopsToOwn: Int = 1) extends ObjectiveCard:
    override def description: String = 
      s"Conquer $num territories with at least $minTroopsToOwn troops each."

  case class ConquerContinents(continents: Set[Continent]) extends ObjectiveCard:
    override def description: String = 
      val continentNames = continents.map(_.name).mkString(", ")
      s"Conquer the following continents: $continentNames."

  case class DefeatPlayer(targetColor: PlayerColor) extends ObjectiveCard:
    override def description: String = 
      s"Defeat the player with ${targetColor.toString.toLowerCase} armies."

  case class ConquerNContinents(n: Int) extends ObjectiveCard:
    override def description: String = 
      s"Conquer any $n continents"