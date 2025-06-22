package strategy

import engine.GameState
import engine.GameAction

trait Strategy:
  
  //decides next Bot move  
  def decideMove(gameState: GameState): GameAction
