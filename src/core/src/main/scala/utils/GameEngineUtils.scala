package utils

import model.player.*
import model.cards.*
import model.board.*
import engine.*
import exceptions._
import scala.util.Random

/**
  * Utility object with helper methods for game engine operations such as card transfer, territory assignment, and victory checks.
  */
object GameEngineUtils:

  /**
    * Draws a territory card for a player.
    * @param gameState The current game state.
    * @param decksManager The decks manager.
    * @param playerId The ID of the player.
    * @return A tuple of the updated game state and decks manager.
    */
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

  /**
    * Checks if a player has any remaining territories.
    * @param gameState The current game state.
    * @param playerId The ID of the player.
    * @return True if the player owns at least one territory, false otherwise.
    */
  def hasRemainingTerritories(gameState: GameState, playerId: String): Boolean = gameState.board.territoriesOwnedBy(playerId).nonEmpty

  /**
    * Counts the number of territories owned by a player.
    * @param gameState The current game state.
    * @param playerId The ID of the player.
    * @return The number of territories owned.
    */
  def countPlayerTerritories(gameState: GameState, playerId: String): Int = gameState.board.territoriesOwnedBy(playerId).size

  /**
    * Gets the adjacent territories owned by a player.
    * @param gameState The current game state.
    * @param territoryName The name of the territory.
    * @param playerId The ID of the player.
    * @return A set of adjacent territories owned by the player.
    */
  def getAdjacentTerritories(gameState: GameState, territoryName: String, playerId: String): Set[Territory] =
    val territory = gameState.getTerritoryByName(territoryName).getOrElse(throw new InvalidTerritoryException())
    territory.neighbors.filter(_.isOwnedBy(playerId)).toSet 

  /**
    * Distributes all territories among players at the start of the game.
    * @param board The game board.
    * @param players The list of players.
    * @return The updated board with territories assigned.
    */
  def distributeInitialTerritories(board: Board, players: List[PlayerImpl]): Board =
    val shuffledTerritories = Random.shuffle(board.territories)
    val assignedTerritories = shuffledTerritories.zipWithIndex.map: 
      case (territory, index) =>
        val playerIndex = index % players.size
        val player = players(playerIndex)
        territory.changeOwner(player).addTroops(1)
    assignedTerritories.foldLeft(board):
      (updatedBoard, territory) => updatedBoard.updatedTerritory(territory)
  
  /**
    * Assigns objectives to each player from the deck.
    * @param currentDecksManager The current decks manager.
    * @param players The list of players.
    * @param playerStates The list of player states.
    * @return A tuple of the updated decks manager and player states.
    */
  def assignObjectivesToPlayers(currentDecksManager: DecksManager, players: List[PlayerImpl], playerStates: List[PlayerState]): (DecksManager, List[PlayerState]) = 
    players.foldLeft((currentDecksManager, List.empty[PlayerState])):
      case ((dm, states), player) =>
        val (updatedDM, objective) = dm.drawObjective()
        val playerState = playerStates.find(_.playerId == player.id).get
        (updatedDM, states :+ playerState.setObjectiveCard(objective))
  
  /**
    * Handles the reward of a territory card after a successful conquest.
    * @param state The current engine state.
    * @param action The action performed.
    * @return The updated engine state.
    */
  def handleTerritoryCardReward(state: EngineState, action: GameAction): EngineState =
    val gameState = state.gameState
    if (action == GameAction.EndTurn && state.territoryConqueredThisTurn)
      val currentPlayerId = gameState.turnManager.currentPlayer.id
      val (updatedGameState, _) = drawTerritoryCard(gameState, gameState.decksManager, currentPlayerId)
      state.copy(gameState = updatedGameState, territoryConqueredThisTurn = false)
    else state

  /**
    * Moves the game to the next player's turn, updating bonuses if needed.
    * @param state The current engine state.
    * @return The updated engine state.
    */
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

  /**
    * Checks if any player has met the victory condition and throws GameOverException if so.
    * @param state The current engine state.
    * @return The engine state if no winner, otherwise throws exception.
    */
  def checkVictoryCondition(state: EngineState): EngineState =
    val optPlayerState = state.gameState.checkWinCondition
    optPlayerState match
      case Some(winner) => throw new GameOverException(winner)
      case None => state