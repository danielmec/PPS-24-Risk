package strategy

import engine.GameState
import engine.GameAction

/**
  * Trait for a bot strategy.
  * A Strategy decides the next move for a bot given the current game state.
  */
trait Strategy:

  /**
    * Decides the next move for the bot.
    * @param gameState The current state of the game.
    * @return The GameAction chosen by the strategy.
    */
  def decideMove(gameState: GameState): GameAction
