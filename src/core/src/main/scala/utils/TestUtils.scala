package utils

import model.player.*
import model.cards.*
import model.board.*
import engine.*

/**
  * Utility object providing helper methods for testing game logic.
  */
object TestUtils:
  /**
    * Assigns a list of territories to a player.
    * @param board The game board.
    * @param territories The territories to assign.
    * @param player The player to assign the territories to.
    * @return The updated board with territories assigned to the player.
    */
  def assignTerritoriesToPlayer(board: Board, territories: Iterable[Territory], player: PlayerImpl): Board =
    territories.foldLeft(board): (b, territory) =>
      b.updatedTerritory(territory.copy(owner = Some(player)))

  /**
    * Updates the bonus troops for a specific player in the list of player states.
    * @param playerStates The list of player states.
    * @param playerId The ID of the player to update.
    * @param bonus The new bonus troop value.
    * @return The updated list of player states.
    */
  def updatePlayerBonus(playerStates: List[PlayerState], playerId: String, bonus: Int): List[PlayerState] =
    playerStates.map:
      case ps if ps.playerId == playerId => ps.copy(bonusTroops = bonus)
      case ps => ps

  /**
    * Resets the turn manager for a new game or test scenario.
    * @param players The list of players.
    * @param phase The phase to set (default is SetupPhase).
    * @return A new TurnManagerImpl instance.
    */
  def resetTurnManager(players: List[PlayerImpl], phase: TurnPhase = TurnPhase.SetupPhase): TurnManagerImpl =
    TurnManagerImpl(
      players = players,
      currentPlayerIndex = 0,
      phase = phase
    )
