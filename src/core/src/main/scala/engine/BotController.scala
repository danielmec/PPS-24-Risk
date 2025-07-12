package engine

import model.player._
import exceptions._

trait BotController:

  def nextAction(gameState: GameState, playerId: String): GameAction