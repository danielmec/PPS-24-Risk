package strategy

import engine.GameState
import model.board.Territory

object RuleHelpers:
  // Trova territori di confine
  def findBorderTerritories(gameState: GameState, playerId: String): Set[Territory] =
    gameState.board.territories
      .filter(_.owner.exists(_.id == playerId))
      .filter(t => hasEnemyNeighbor(t, gameState, playerId))
      .toSet
  
  // Verifica se un territorio ha vicini nemici
  def hasEnemyNeighbor(territory: Territory, gameState: GameState, playerId: String): Boolean =
    territory.neighbors.exists(n => 
      n.owner.exists(_.id != playerId) && n.owner.isDefined
    )
    
  // Calcola il livello di minaccia di un territorio
  def calculateThreatLevel(territory: Territory, gameState: GameState): Double =
    val enemyNeighborTroops = territory.neighbors
      .filter(n => n.owner.exists(o => o.id != territory.owner.get.id))
      .map(_.troops)
      .sum
    val ownTroops = territory.troops
    if ownTroops == 1 then Double.MaxValue  // Territorio vuoto: priorità massima
    else enemyNeighborTroops.toDouble / ownTroops