package engine

import model.player._
import exceptions._

/**
  * Trait for a controller that manages bot actions in the game.
  */
trait BotController:

  /**
    * Returns the next action for the bot player with the given ID, given the current game state.
    * @param gameState The current state of the game.
    * @param playerId The unique identifier of the bot player.
    * @return The next GameAction to be performed by the bot.
    */
  def nextAction(gameState: GameState, playerId: String): GameAction