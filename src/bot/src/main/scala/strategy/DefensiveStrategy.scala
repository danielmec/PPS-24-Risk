package strategy

import engine.{GameState, TurnPhase}
import model.player.PlayerState
import model.board.Territory
import model.cards.TerritoryCard
import utils.BonusCalculator

class DefensiveStrategy extends Strategy:

  override def decideMove(gameState: GameState): BotAction =
    // Trova lo stato del player corrente
    val maybePlayerState = gameState.playerStates.find(_.player == gameState.turnManager.currentPlayer)
    maybePlayerState match
      case Some(playerState) =>
        gameState.turnManager.currentPhase match
          case TurnPhase.Reinforcement | TurnPhase.PlacingTroops =>
            if shouldTradeCards(playerState) then
              BotAction.TradeCards(selectCardsToTrade(playerState))
            else if playerState.bonusTroops > 0 then
              val owned = gameState.board.territories.filter(_.owner.contains(playerState.player))
              val weakest = findWeakestTerritory(owned)
              BotAction.PlaceTroops(weakest, playerState.bonusTroops)
            else
              BotAction.EndPhase

          case TurnPhase.Attacking =>
            BotAction.EndPhase

          case TurnPhase.Defending =>
            BotAction.EndPhase

          case TurnPhase.WaitingForTurn =>
            BotAction.EndPhase

      case None => BotAction.EndPhase

  // --- Helper methods ---

  private def shouldTradeCards(playerState: PlayerState): Boolean =
    playerState.territoryCards.size >= 3 &&
      playerState.territoryCards.subsets(3).exists(tris => BonusCalculator.calculateTradeBonus(tris) > 0)

  private def selectCardsToTrade(playerState: PlayerState): Set[TerritoryCard] =
    playerState.territoryCards.subsets(3)
      .find(tris => BonusCalculator.calculateTradeBonus(tris) > 0)
      .getOrElse(playerState.territoryCards.take(3).toSet)

  private def findWeakestTerritory(territories: Iterable[Territory]): Territory =
    territories.minBy(_.troops)
