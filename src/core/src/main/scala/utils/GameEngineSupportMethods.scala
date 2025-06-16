package utils

import model.player.*
import model.cards.*
import model.board.*
import engine.*

object GameEngineUtils:
  
  def transferCardsOnElimination(
    gameState: GameState, 
    eliminatedPlayerId: String, 
    attackerId: String
  ): GameState =
    val eliminatedPlayerState = gameState.playerStates.find(_.playerId == eliminatedPlayerId)
    val attackerPlayerState = gameState.playerStates.find(_.playerId == attackerId)
    
    (eliminatedPlayerState, attackerPlayerState) match
      case (Some(eliminated), Some(attacker)) =>
        val updatedAttacker = attacker.copy(
          territoryCards = attacker.territoryCards ++ eliminated.territoryCards
        )
        val updatedEliminated = eliminated.copy(territoryCards = Set.empty)
        
        val updatedPlayerStates = gameState.playerStates.map:
          case ps if ps.playerId == attackerId => updatedAttacker
          case ps if ps.playerId == eliminatedPlayerId => updatedEliminated
          case ps => ps
        
        gameState.copy(playerStates = updatedPlayerStates)
      case _ => gameState

  def drawTerritoryCard(
    gameState: GameState, 
    decksManager: DecksManager, 
    playerId: String
  ): (GameState, DecksManager) =
    try
      val (updatedDecksManager, card) = decksManager.drawTerritory()
      val updatedPlayerStates = gameState.playerStates.map:
        case ps if ps.playerId == playerId => 
          ps.copy(territoryCards = ps.territoryCards + card)
        case ps => ps
      val updatedGameState = gameState.copy(playerStates = updatedPlayerStates)
      (updatedGameState, updatedDecksManager)
    catch
      case _: Exception => 
        (gameState, decksManager)

  def hasRemainingTerritories(gameState: GameState, playerId: String): Boolean =
    gameState.board.territories.exists(_.owner.exists(_.id == playerId))

  def countPlayerTerritories(gameState: GameState, playerId: String): Int =
    gameState.board.territories.count(_.owner.exists(_.id == playerId))

  def getAdjacentTerritories(gameState: GameState, territoryName: String, playerId: String): Set[Territory] =
    gameState.board.territories.find(_.name == territoryName) match
      case Some(territory) if territory.owner.exists(_.id == playerId) =>
        territory.neighbors.filter(_.owner.exists(_.id == playerId))
      case _ => Set.empty