package bot

import engine._
import strategy._
import model.player._
import scala.collection.mutable

class RiskBotController extends BotController:
  private val botStrategies = mutable.Map[String, Strategy]()
  
  /**
   * Registra una strategia per un giocatore bot
   */
  def registerStrategy(playerId: String, strategy: Strategy): Unit =
    botStrategies.put(playerId, strategy)
  
  /**
   * Ottiene la prossima azione del bot in base alla sua strategia
   */
  override def nextAction(gameState: GameState, playerId: String): GameAction =
    botStrategies.get(playerId) match
      case Some(strategy) => strategy.decideMove(gameState)
      case None => throw new IllegalStateException(s"No strategy registered for bot $playerId")