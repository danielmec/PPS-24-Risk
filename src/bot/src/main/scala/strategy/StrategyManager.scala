package strategy

import engine.GameState
import bot.BotConfig
import model.board.Territory

/**
 * StrategyManager valuta lo stato della partita e suggerisce la strategia migliore per il bot.
 */
class StrategyManager(config: BotConfig):

  private val MIN_TERRITORIES_FOR_AGGRESSIVE = 6
  private val MIN_AVERAGE_TROOPS_FOR_AGGRESSIVE = 3.0
  private val MAX_TERRITORIES_FOR_DEFENSIVE = 3
  private val MIN_AVERAGE_TROOPS_FOR_DEFENSIVE = 2.0
  private val MIN_TROOPS_FOR_TERRITORY = 1

  /**
   * Valuta se cambiare strategia. Restituisce Some(nuovaStrategia) se serve cambiare,
   * altrimenti None per mantenere quella attuale.
   */
  def evaluateStrategy(gameState: GameState, currentStrategy: Strategy): Option[Strategy] =
    val allTerritories = gameState.board.territories.toSeq
    val ownedTerritories = allTerritories.filter(_.isOwnedBy(config.name))
    val totalOwned = ownedTerritories.size
    val totalTroops = ownedTerritories.map(_.troops).sum
    val averageTroopsPerTerritory =
      if totalOwned >= MIN_TROOPS_FOR_TERRITORY then totalTroops.toDouble / totalOwned else 0.0

    if (totalOwned <= MAX_TERRITORIES_FOR_DEFENSIVE || averageTroopsPerTerritory < MIN_AVERAGE_TROOPS_FOR_DEFENSIVE)
        && !currentStrategy.isInstanceOf[DefensiveStrategy] then
      Some(new DefensiveStrategy())
    else if (totalOwned >= MIN_TERRITORIES_FOR_AGGRESSIVE && averageTroopsPerTerritory >= MIN_AVERAGE_TROOPS_FOR_AGGRESSIVE)
        && !currentStrategy.isInstanceOf[AggressiveStrategy] then
      Some(new AggressiveStrategy())
    else
      None

