package model.cards

import model.player.PlayerColor
import model.board.Continent

sealed trait ObjectiveCard:
  def description: String

object ObjectiveCard:

  case class ConquerTerritories(num: Int, minTroopsToOwn: Int = 1) extends ObjectiveCard:
    override def description: String = 
      s"Conquista $num territori con almeno $minTroopsToOwn truppe."

  case class ConquerContinents(continents: Set[Continent]) extends ObjectiveCard:
    override def description: String = 
      val continentNames = continents.map(_.name).mkString(", ")
      s"Conquista i seguenti continenti: $continentNames."

  case class ConquerNContinents(n: Int) extends ObjectiveCard:
    override def description: String = 
      s"Conquista $n continenti."