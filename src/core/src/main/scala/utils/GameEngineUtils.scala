package utils

import model.player.*
import model.cards.*
import model.board.*
import engine.*
import exceptions._
import scala.util.Random

object GameEngineUtils:
  
  def transferCardsOnElimination(
    gameState: GameState, 
    eliminatedPlayerId: String,
    conquererPlayerId: String
  ): GameState = 
    val eliminatedPlayer = gameState.getPlayerState(eliminatedPlayerId).getOrElse(throw new InvalidPlayerException())
    val conquererPlayer = gameState.getPlayerState(conquererPlayerId).getOrElse(throw new InvalidPlayerException())
    val eliminatedCards = eliminatedPlayer.territoryCards
    val updatedConquerState = conquererPlayer.addTerritoryCards(eliminatedCards)
    gameState
      .updatePlayerState(conquererPlayerId, updatedConquerState)
      .updatePlayerState(eliminatedPlayerId, eliminatedPlayer.removeTerritoryCards(eliminatedPlayer.territoryCards))

  def drawTerritoryCard(
    gameState: GameState,
    decksManager: DecksManager,
    playerId: String
  ): (GameState, DecksManager) = 
    try {
      val (updatedDecksManager, card) = decksManager.drawTerritory()
      val playerState = gameState.getPlayerState(playerId).getOrElse(throw new InvalidPlayerException())
      val updatedPlayerState = playerState.addTerritoryCard(card)     
      val updatedGameState = gameState
        .updatePlayerState(playerId, updatedPlayerState)
        .updateDecksManager(updatedDecksManager)    
      (updatedGameState, updatedDecksManager)
    } catch {
      case e: Exception => (gameState, decksManager)
    }

  def hasRemainingTerritories(gameState: GameState, playerId: String): Boolean = gameState.board.territoriesOwnedBy(playerId).nonEmpty

  def countPlayerTerritories(gameState: GameState, playerId: String): Int = gameState.board.territoriesOwnedBy(playerId).size

  def getAdjacentTerritories(gameState: GameState, territoryName: String, playerId: String): Set[Territory] =
    val territory = gameState.getTerritoryByName(territoryName).getOrElse(throw new InvalidTerritoryException())
    territory.neighbors.filter(_.isOwnedBy(playerId)).toSet 

  def distributeInitialTerritories(board: Board, players: List[PlayerImpl]): Board =
    val shuffledTerritories = Random.shuffle(board.territories)
    val assignedTerritories = shuffledTerritories.zipWithIndex.map: 
      case (territory, index) =>
        val playerIndex = index % players.size
        val player = players(playerIndex)
        territory.changeOwner(player).addTroops(1)
    assignedTerritories.foldLeft(board):
      (updatedBoard, territory) => updatedBoard.updatedTerritory(territory)
  
  def assignObjectivesToPlayers(currentDecksManager: DecksManager, players: List[PlayerImpl], playerStates: List[PlayerState]): (DecksManager, List[PlayerState]) = 
    players.foldLeft((currentDecksManager, List.empty[PlayerState])):
      case ((dm, states), player) =>
        val (updatedDM, objective) = dm.drawObjective()
        val playerState = playerStates.find(_.playerId == player.id).get
        (updatedDM, states :+ playerState.setObjectiveCard(objective))
  
  def handleTerritoryCardReward(state: EngineState, action: GameAction): EngineState =
    val gameState = state.gameState
    if (action == GameAction.EndTurn && state.territoryConqueredThisTurn)
      val currentPlayerId = gameState.turnManager.currentPlayer.id
      val (updatedGameState, _) = drawTerritoryCard(gameState, gameState.decksManager, currentPlayerId)
      state.copy(gameState = updatedGameState, territoryConqueredThisTurn = false)
    else state

  def moveToNextPlayer(state: EngineState): EngineState =
    val gameState = state.gameState
    val nextTurnManager = gameState.turnManager.nextPlayer()
    var updatedGameState = gameState.updateTurnManager(nextTurnManager)
    if (nextTurnManager.currentPhase == TurnPhase.MainPhase) 
      val nextPlayerId = nextTurnManager.currentPlayer.id
      val nextPlayerState = updatedGameState.getPlayerState(nextPlayerId).get
      val bonus = BonusCalculator.calculateStartTurnBonus(nextPlayerId, updatedGameState.board)
      val updatedPlayerState = nextPlayerState.copy(bonusTroops = bonus)
      updatedGameState = updatedGameState.updatePlayerState(nextPlayerId, updatedPlayerState)
    state.copy(
      gameState = updatedGameState,
      territoryConqueredThisTurn = state.territoryConqueredThisTurn
    )

  def checkVictoryCondition(state: EngineState): EngineState =
    val optPlayerState = state.gameState.checkWinCondition
    optPlayerState match
      case Some(winner) => throw new GameOverException(winner)
      case None => state