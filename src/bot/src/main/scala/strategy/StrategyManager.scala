package bot.strategy

import bot.{BotConfig, GameState}

/**
 * Manager che si occupa di valutare e cambiare le strategie del bot
 * in base allo stato del gioco e alla sua configurazione
 */
class StrategyManager(config: BotConfig):
  private val MinTerritoriesForAggressive = 10
  private val MinArmiesPerTerritoryForAggressive = 3
  private val MaxBorderingEnemiesForAggressive = 3
  
  def evaluateStrategy(gameState: GameState): Strategy =
    if needsDefensiveStrategy(gameState) then
      new DefensiveStrategy()
    else if canBeAggressive(gameState) then
      new AggressiveStrategy()
    else
      new RandomStrategy()
    
  private def needsDefensiveStrategy(gameState: GameState): Boolean = 
    val ownedTerritories = gameState.getTerritoriesOwnedBy(config.name)
    val totalArmies = gameState.getTotalArmiesFor(config.name)
    val averageArmiesPerTerritory = totalArmies / ownedTerritories.size
    
    // Passa a difensiva se:
    // - Ha pochi territori (<= 5)
    // - Media armate per territorio bassa (<= 2)
    // - Molti nemici confinanti (> 3 per territorio)
    ownedTerritories.size <= 5 ||
    averageArmiesPerTerritory <= 2 ||
    hasHighEnemyPressure(gameState)
    
  private def canBeAggressive(gameState: GameState): Boolean = 
    val ownedTerritories = gameState.getTerritoriesOwnedBy(config.name)
    val totalArmies = gameState.getTotalArmiesFor(config.name)
    val averageArmiesPerTerritory = totalArmies / ownedTerritories.size
    
    // Passa ad aggressiva se:
    // - Ha molti territori (>= 10)
    // - Media armate per territorio alta (>= 3)
    // - Pochi nemici confinanti (<= 3 per territorio)
    // - Configurato con alta aggressività (> 0.7)
    ownedTerritories.size >= MinTerritoriesForAggressive &&
    averageArmiesPerTerritory >= MinArmiesPerTerritoryForAggressive &&
    !hasHighEnemyPressure(gameState) &&
    config.aggressiveness > 0.7
    
  private def hasHighEnemyPressure(gameState: GameState): Boolean =
    val borderTerritories = gameState.getBorderingEnemyTerritories(config.name)
    borderTerritories.size > MaxBorderingEnemiesForAggressive

