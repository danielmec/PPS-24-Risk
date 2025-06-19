package strategy

import engine.GameState
import engine.GameAction

/**
 * Trait che definisce il comportamento di una strategia per il bot
 */
trait Strategy:
  /**
   * Decide la prossima mossa del bot in base allo stato del gioco
   * @param gameState stato corrente del gioco
   * @return lista di azioni che il bot deve eseguire
   */
  def decideMove(gameState: GameState): GameAction