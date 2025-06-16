package strategy

import model.board.Territory
import engine.GameState

class DefensiveStrategy extends Strategy:
  override def decideMove(gameState: GameState): BotAction =
    val playerOpt = gameState.getPlayerState(gameState.turnManager.currentPlayer.id)
    playerOpt match
      case Some(playerState) =>
        val ownedTerritories = gameState.board.territories.filter(_.isOwnedBy(playerState.playerId))
        if ownedTerritories.nonEmpty && playerState.troopsToDeploy > 0 then
          val weakestTerritory = ownedTerritories.minBy(_.troops)
          BotAction.PlaceTroops(weakestTerritory, playerState.troopsToDeploy)
        else
          BotAction.EndPhase
      case None => BotAction.EndPhase
