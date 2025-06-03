package model.cards

import model.player.PlayerColor
import model.board.Continent
import engine.GameState

sealed trait ObjectiveCard extends Card:
  def done(gameState: GameState): Boolean
  def description: String

object ObjectiveCard:

  case class ConquerTerritories(num: Int, minTroopsToOwn: Int = 1) extends ObjectiveCard:
    
    override def description: String = 
      s"Conquer $num territories with at least $minTroopsToOwn troops each."

    override def done(gameState: GameState): Boolean = 
      val currentPlayerId = gameState.turnManager.currentPlayer.id
      val ownedTerritories = gameState.board.territories.count(
        territory  =>
          territory.isOwnedBy(currentPlayerId) && territory.troops >= minTroopsToOwn
      )
      ownedTerritories >= num


  case class ConquerContinents(continents: Set[Continent]) extends ObjectiveCard:

    override def description: String = 
      val continentNames = continents.map(_.name).mkString(", ")
      s"Conquer the following continents: $continentNames."

    override def done(gameState: GameState): Boolean = 
      val currentPlayerId = gameState.turnManager.currentPlayer.id
      continents.forall(continent => continent.territories.forall(_.isOwnedBy(currentPlayerId)))

    
  case class DefeatPlayer(targetColor: PlayerColor) extends ObjectiveCard:

    override def description: String = 
      s"Eliminate the player with ${targetColor.toString.toLowerCase} armies."

    override def done(gameState: GameState): Boolean = 
      val target = gameState.playerStates.find(_.playerColor == targetColor)
      target.isEmpty ||
        !gameState.board.territories.exists(_.isOwnedBy(target.get.playerId))


  case class ConquerNContinents(n: Int) extends ObjectiveCard:

    override def description: String = s"Conquer $n continents."

    override def done(gameState: GameState): Boolean = 
      val currentPlayerId = gameState.turnManager.currentPlayer.id
      val conqueredContinents = gameState.board.continents.count(
        continent =>
          continent.territories.forall(_.isOwnedBy(currentPlayerId))
      )
      conqueredContinents >= n