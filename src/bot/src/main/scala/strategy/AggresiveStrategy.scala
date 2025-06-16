package strategy

import model.board.Territory
import engine.GameState

class AggressiveStrategy extends Strategy:
  override def decideMove(gameState: GameState): BotAction =
    val playerOpt = gameState.getPlayerState(gameState.turnManager.currentPlayer.id)
    playerOpt match
      case Some(playerState) =>
        val owned = gameState.board.territories.filter(_.isOwnedBy(playerState.playerId)).toSeq
        val candidates = owned.filter(_.troops > 1)

        val attackOption = candidates.flatMap { from =>
          from.neighbors
            .filterNot(_.isOwnedBy(playerState.playerId))
            .map(to => (from, to))
        }.sortBy { case (_, to) => to.troops }.headOption

        attackOption match
          case Some((from, to)) =>
            val attackingTroops = (from.troops - 1).min(3)
            BotAction.Attack(from, to, attackingTroops)
          case None => BotAction.EndPhase

      case None => BotAction.EndPhase
