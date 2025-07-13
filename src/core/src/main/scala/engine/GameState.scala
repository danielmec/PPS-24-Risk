package engine

import model.cards.* 
import model.player.*
import model.board.*

/**
  * Represents the complete state of the game at a given moment.
  *
  * @param gameId The unique identifier of the game.
  * @param board The current board state.
  * @param playerStates The list of player states.
  * @param turnManager The turn manager for the game.
  * @param decksManager The decks manager for the game.
  * @param objectiveCards The list of objective cards in play.
  * @param lastBattleResult The result of the last battle round, if any.
  * @param playerStartedTurn True if the player has started their turn.
  */
case class GameState(
    gameId: String,
    board: Board,
    playerStates: List[PlayerState],
    turnManager: TurnManager,
    decksManager: DecksManager,
    objectiveCards: List[ObjectiveCard] = List.empty,
    lastBattleResult: Option[BattleRoundResult] = None,
    playerStartedTurn: Boolean = true  
):
  /**
    * Returns the list of territories on the board.
    */
  def territories: List[Territory] = board.territories
  
  /**
    * Gets the state of the player with the given ID.
    * @param playerId The unique identifier of the player.
    * @return Some(PlayerState) if found, None otherwise.
    */
  def getPlayerState(playerId: String): Option[PlayerState] =
    playerStates.find(_.playerId == playerId)
  
  /**
    * Updates the state of the player with the given ID.
    * @param playerId The unique identifier of the player.
    * @param newState The new state for the player.
    * @return The updated GameState.
    */
  def updatePlayerState(playerId: String, newState: PlayerState): GameState =
    copy(playerStates = playerStates.map {
      case ps if ps.playerId == playerId => newState
      case ps => ps
    })
    
  /**
    * Updates the board with a new board state.
    * @param newBoard The new board.
    * @return The updated GameState.
    */
  def updateBoard(newBoard: Board): GameState =
    copy(board = newBoard)
    
  /**
    * Updates the turn manager with a new instance.
    * @param newTurnManager The new turn manager.
    * @return The updated GameState.
    */
  def updateTurnManager(newTurnManager: TurnManager): GameState =
    copy(turnManager = newTurnManager)
    
  /**
    * Updates the decks manager with a new instance.
    * @param newDecksManager The new decks manager.
    * @return The updated GameState.
    */
  def updateDecksManager(newDecksManager: DecksManager): GameState =
    copy(decksManager = newDecksManager)
  
  /**
    * Updates the last battle result.
    * @param battleResult The result of the last battle round.
    * @return The updated GameState.
    */
  def updateLastBattleResult(battleResult: BattleRoundResult): GameState =
    copy(lastBattleResult = Some(battleResult))
    
  /**
    * Gets a territory by its name.
    * @param name The name of the territory.
    * @return Some(Territory) if found, None otherwise.
    */
  def getTerritoryByName(name: String): Option[Territory] =
    board.getTerritoryByName(name)
    
  /**
    * Checks if any player has completed their objective and returns the winner if so.
    * @return Some(PlayerState) of the winner, or None if no winner.
    */
  def checkWinCondition: Option[PlayerState] =
    playerStates.find(playerState => 
      ObjectiveValidator.done(playerState.objectiveCard, this, playerState)
    )
