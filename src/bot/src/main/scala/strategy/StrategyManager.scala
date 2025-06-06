package bot.strategy

import bot.{BotConfig, GameState}

/**
 * Manager che si occupa di valutare e cambiare le strategie del bot
 * in base allo stato del gioco e alla sua configurazione
 */
class StrategyManager(config: BotConfig):
  val MinTerritoriesForAggressive = 10
  val MinArmiesPerTerritoryForAggressive = 3
  val MaxBorderingEnemiesForAggressive = 3

  def evaluateStrategy(gameState: GameState): Option[Strategy] = //option perchè non è detto che si cambi strategia
    if needsDefensiveStrategy(gameState) then
      Some(new DefensiveStrategy())
    else if canBeAggressive(gameState) then
      Some(new AggressiveStrategy())
    else
      None


  def needsDefensiveStrategy(gameState: GameState): Boolean = 
    val ownedTerritories = gameState.getTerritoriesOwnedBy(config.name)
    val totalArmies = gameState.getTotalArmiesFor(config.name)
    val averageArmiesPerTerritory = totalArmies / ownedTerritories.size
    
    //passa a difensiva se:
    // - Ha pochi territori (<= 5)
    // - Media armate per territorio bassa (<= 2)
    // - Molti nemici confinanti (> 3 per territorio)
    ownedTerritories.size <= 5 ||
    averageArmiesPerTerritory <= 2 ||
    hasHighEnemyPressure(gameState)
    
  def canBeAggressive(gameState: GameState): Boolean = 
    val ownedTerritories = gameState.getTerritoriesOwnedBy(config.name)
    val totalArmies = gameState.getTotalArmiesFor(config.name)
    val averageArmiesPerTerritory = totalArmies / ownedTerritories.size
    
    //passa ad aggressiva se:
    // - Ha molti territori (>= 10)
    // - Media armate per territorio alta (>= 3)
    // - Pochi nemici confinanti (<= 3 per territorio)
    ownedTerritories.size >= MinTerritoriesForAggressive &&
    averageArmiesPerTerritory >= MinArmiesPerTerritoryForAggressive &&
    !hasHighEnemyPressure(gameState) 
    
  def hasHighEnemyPressure(gameState: GameState): Boolean =
    val borderTerritories = gameState.getBorderingEnemyTerritories(config.name)
    borderTerritories.size > MaxBorderingEnemiesForAggressive

