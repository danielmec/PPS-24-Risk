package strategy

import engine.GameState
import engine.TurnPhase
import model.player.PlayerState
import model.board.Territory
import model.cards.TerritoryCard
import utils.BonusCalculator

class AggressiveStrategy extends Strategy:

  override def decideMove(gameState: GameState): BotAction =
    val maybePlayerState = gameState.playerStates.find(_.player == gameState.turnManager.currentPlayer)
    maybePlayerState match
      case Some(playerState) =>
        gameState.turnManager.currentPhase match
          case TurnPhase.Reinforcement | TurnPhase.PlacingTroops =>
            if shouldTradeCards(playerState) then
              BotAction.TradeCards(selectCardsToTrade(playerState))
            else if playerState.bonusTroops > 0 then
              val owned = gameState.board.territories.filter(_.owner.contains(playerState.player))
              val strongest = findStrongestTerritory(owned)
              BotAction.PlaceTroops(strongest, playerState.bonusTroops)
            else
              BotAction.EndPhase

          case TurnPhase.Attacking =>
            decideAttack(gameState, playerState)

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

  private def findStrongestTerritory(territories: Iterable[Territory]): Territory =
    territories.maxBy(_.troops)

  // Attacco aggressivo: attacca sempre se possibile, scegliendo il vicino più debole
  private def decideAttack(gameState: GameState, playerState: PlayerState): BotAction =
    val owned = gameState.board.territories.filter(_.owner.exists(_ == playerState.player))
    val candidates = owned.filter(_.troops > 1)

    // Per ogni territorio candidato, trova i vicini attaccabili (cioè con owner diverso)
    val possibleAttacks = for {
      from <- candidates
      to <- from.neighbors
      // Trova il vero oggetto Territory dal board per il vicino (per evitare problemi di istanza)
      realTo <- gameState.board.territories.find(_.name == to.name)
      if realTo.owner.exists(_ != playerState.player)
    } yield (from, realTo)

    // Ordina per il numero di truppe del difensore (attacca il più debole)
    val attackOption = possibleAttacks.toSeq.sortBy { case (_, to) => to.troops }.headOption

    attackOption match
      case Some((from, to)) =>
        val attackingTroops = (from.troops - 1).min(3)
        BotAction.Attack(from, to, attackingTroops)
      case None =>
        BotAction.EndAttack