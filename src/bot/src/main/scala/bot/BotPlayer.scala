package bot

import strategy.Strategy
import strategy.StrategyRule
import engine.GameState
import model.player.{Player, PlayerColor, PlayerType}
import engine.TurnPhase
import engine.GameAction

//////////////////////////""""DEFINITIVO"""""/////////////////////////////

class BotPlayer(playerId: String, strategyRules: Set[StrategyRule]) extends Strategy:
  override def decideMove(gameState: GameState): GameAction = 
    val ratedActions = strategyRules.evaluateAction(gameState, playerId)

    if ratedActions.isEmpty then
      //nessuna azione -> termina turno
      gameState.turnManager.currentPhase match
        case TurnPhase.Attacking => GameAction.EndAttack
        case TurnPhase.Reinforcement => GameAction.EndTurn
        case _ => GameAction.EndPhase
    else 
      ratedActions.max.action
      