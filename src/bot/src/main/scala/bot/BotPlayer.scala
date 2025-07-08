package bot

import strategy.Strategy
import strategy.StrategyRule
import engine.GameState
import model.player.Player
import model.player.PlayerType
import model.player.PlayerColor
import engine.TurnPhase
import engine.GameAction
import prolog._

class BotPlayer(
    override val id: String,
    override val name: String,
    override val color: PlayerColor,

    val strategyRules: Set[StrategyRule]

) extends Player, Strategy:
  override def playerType: PlayerType = PlayerType.Bot

  override def decideMove(gameState: GameState): GameAction =
    val currentPhase = gameState.turnManager.currentPhase
    val currentPlayerState = gameState.getPlayerState(id).get
    
    val rulesToApply = currentPhase match
      case TurnPhase.SetupPhase => strategyRules.filter(_.isInstanceOf[BotSetupPlaceTroopsRule])
      case TurnPhase.MainPhase => 
        if (currentPlayerState.bonusTroops > 0) then strategyRules.filter(_.isInstanceOf[OffensiveBotPlaceTroopsRule])
        else  
          val attackActions = strategyRules.filter(_.isInstanceOf[OffensiveBotAttackRule]).evaluateAction(gameState, id)
          if attackActions.nonEmpty then return attackActions.max.action 
          val reinforceActions = strategyRules.filter(_.isInstanceOf[OffensiveBotReinforceRule]).evaluateAction(gameState, id)
          if reinforceActions.nonEmpty then return reinforceActions.max.action
          return GameAction.EndTurn   // no attacks or reinforces available
  
    val ratedActions = rulesToApply.evaluateAction(gameState, id)
    
    if ratedActions.isEmpty then
      currentPhase match
        case TurnPhase.SetupPhase => GameAction.EndSetup
        case TurnPhase.MainPhase  => GameAction.EndTurn
    else ratedActions.max.action