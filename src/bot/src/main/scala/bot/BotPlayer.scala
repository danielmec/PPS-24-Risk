package bot

import strategy.Strategy
import strategy.StrategyRule
import engine.GameState
import model.player.Player
import model.player.PlayerType
import model.player.PlayerColor
import engine.TurnPhase
import engine.GameAction

class BotPlayer(
    override val id: String,
    override val name: String,
    override val color: PlayerColor,
    val strategyRules: Set[StrategyRule]
) extends Player, Strategy:
  override def playerType: PlayerType = PlayerType.Bot

  override def decideMove(gameState: GameState): GameAction =
    val ratedActions = strategyRules.evaluateAction(gameState, id)
    //nessuna azione -> termina turno
    if ratedActions.isEmpty then
      gameState.turnManager.currentPhase match
        case TurnPhase.Attacking => GameAction.EndAttack
        case TurnPhase.Reinforcement => GameAction.EndTurn
        case _ => GameAction.EndPhase
    else
      ratedActions.max.action
