package bot

import engine._
import strategy._
import model.player._
import scala.collection.mutable

/**
  * Controller for managing bot strategies and actions in the game.
  * Allows registering strategies for bots and retrieving their next action.
  */
class RiskBotController extends BotController:

  private val botStrategies = mutable.Map[String, Strategy]()

  /**
    * Registers a strategy for a bot player.
    * @param playerId The unique identifier of the bot player.
    * @param strategy The strategy to associate with the bot.
    */
  def registerStrategy(playerId: String, strategy: Strategy): Unit =
    botStrategies.put(playerId, strategy)

  /**
    * Gets the next action for the bot based on its registered strategy.
    * @param gameState The current state of the game.
    * @param playerId The unique identifier of the bot player.
    * @return The next GameAction decided by the bot's strategy.
    * @throws IllegalStateException if no strategy is registered for the bot.
    */
  override def nextAction(gameState: GameState, playerId: String): GameAction =
    botStrategies.get(playerId) match
      case Some(strategy) => strategy.decideMove(gameState)
      case None => throw new IllegalStateException(s"No strategy registered for bot $playerId")