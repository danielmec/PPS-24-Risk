package utils

import model.player.*
import model.cards.*
import model.board.*
import engine.*

object GameEngineUtils:
  
  def transferCardsOnElimination(
    gameState: GameState, 
    eliminatedPlayerId: String,
    conquererPlayerId: String
  ): GameState = {
    val eliminatedPlayer = gameState.playerStates.find(_.playerId == eliminatedPlayerId).getOrElse(
      throw new IllegalArgumentException(s"Player $eliminatedPlayerId not found")
    )
    val conquererPlayer = gameState.playerStates.find(_.playerId == conquererPlayerId).getOrElse(
      throw new IllegalArgumentException(s"Player $conquererPlayerId not found")
    )
    val eliminatedCards = eliminatedPlayer.territoryCards
    val updatedConquerState = conquererPlayer.copy(
      territoryCards = conquererPlayer.territoryCards ++ eliminatedCards
    )
    gameState
      .updatePlayerState(conquererPlayerId, updatedConquerState)
      .updatePlayerState(eliminatedPlayerId, eliminatedPlayer.copy(territoryCards = Set.empty))
  }

  def drawTerritoryCard(
    gameState: GameState,
    decksManager: DecksManager,
    playerId: String
  ): (GameState, DecksManager) = {
    try {
      val (updatedDecksManager, card) = decksManager.drawTerritory()
      val playerState = gameState.playerStates.find(_.playerId == playerId).getOrElse(
        throw new IllegalArgumentException(s"Player $playerId not found")
      )
      val updatedPlayerState = playerState.copy(
        territoryCards = playerState.territoryCards + card
      )     
      val updatedGameState = gameState
        .updatePlayerState(playerId, updatedPlayerState)
        .updateDecksManager(updatedDecksManager)    
      (updatedGameState, updatedDecksManager)
    } catch {
      case e: Exception => (gameState, decksManager)
    }
  }

  def hasRemainingTerritories(gameState: GameState, playerId: String): Boolean =
    gameState.board.territories.exists(_.owner.exists(_.id == playerId))

  def countPlayerTerritories(gameState: GameState, playerId: String): Int =
    gameState.board.territories.count(_.owner.exists(_.id == playerId))

  def getAdjacentTerritories(gameState: GameState, territoryName: String, playerId: String): Set[Territory] = {
    val territory = gameState.board.territories.find(_.name == territoryName).getOrElse(
      throw new IllegalArgumentException(s"Territory $territoryName not found")
    )
    territory.neighbors.filter(_.owner.exists(_.id == playerId)).toSet
  }