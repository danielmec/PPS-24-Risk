package utils

import model.player.*
import model.cards.*
import model.board.*
import engine.*

object TestUtils:
  def assignTerritoriesToPlayer(board: Board, territories: Iterable[Territory], player: PlayerImpl): Board =
    territories.foldLeft(board): (b, territory) =>
      b.updatedTerritory(territory.copy(owner = Some(player)))

  def updatePlayerBonus(playerStates: List[PlayerState], playerId: String, bonus: Int): List[PlayerState] =
    playerStates.map:
      case ps if ps.playerId == playerId => ps.copy(bonusTroops = bonus)
      case ps => ps

  def resetTurnManager(players: List[PlayerImpl], phase: TurnPhase = TurnPhase.WaitingForTurn): TurnManagerImpl =
    TurnManagerImpl(
      players = players,
      currentPlayerIndex = 0,
      phase = phase
    )
