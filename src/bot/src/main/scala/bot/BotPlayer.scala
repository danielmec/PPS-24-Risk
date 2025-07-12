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
  
  private val isOffensive: Boolean = 
    strategyRules.exists(_.isInstanceOf[OffensiveBotAttackRule])
  
  /**
   * Decides the next move based on the current game state and the bot's strategy.
   * It evaluates the available actions according to the rules defined in `strategyRules`.
   * 
   * @param gameState The current state of the game.
   * @return The action decided by the bot.
   */
  override def decideMove(gameState: GameState): GameAction =
    val currentPlayerState = gameState.getPlayerState(id).get
    
    val phaseAppropriateRules = gameState.turnManager.currentPhase match
      case TurnPhase.SetupPhase => 
        val setupRules = strategyRules.filter(_.isInstanceOf[BotSetupPlaceTroopsRule])
        setupRules
        
      case TurnPhase.MainPhase => 
        if currentPlayerState.bonusTroops > 0 then
          val placeTroopsRules = 
            if (isOffensive) then strategyRules.filter(_.isInstanceOf[OffensiveBotPlaceTroopsRule])
            else strategyRules.filter(_.isInstanceOf[DefensiveBotPlaceTroopsRule])        
          placeTroopsRules
        else
          val attackRules = 
            if (isOffensive) then strategyRules.filter(_.isInstanceOf[OffensiveBotAttackRule])
            else strategyRules.filter(_.isInstanceOf[DefensiveBotAttackRule])
          val attackActions = attackRules.evaluateAction(gameState, id)
          if attackActions.nonEmpty then
            val bestAttack = attackActions.max
            println(s"[BOT $name] Trovati ${attackActions.size} attacchi validi, eseguo: ${bestAttack.action}")
            bestAttack.action match
              case _: GameAction.Attack => Thread.sleep(3000)
              case _ =>
            return bestAttack.action
          
          val reinforceRules = 
            if (isOffensive) then strategyRules.filter(_.isInstanceOf[OffensiveBotReinforceRule])
            else strategyRules.filter(_.isInstanceOf[DefensiveBotReinforceRule])
          val reinforceActions = reinforceRules.evaluateAction(gameState, id)
          
          if reinforceActions.nonEmpty then
            println(s"[BOT $name] Trovati ${reinforceActions.size} rinforzi validi, scelgo il migliore")
            return reinforceActions.max.action
          
          println(s"[BOT $name] Nessuna azione valida, termino il turno")
          return GameAction.EndTurn
  
    val ratedActions = phaseAppropriateRules.evaluateAction(gameState, id)
    if ratedActions.isEmpty then
      gameState.turnManager.currentPhase match
        case TurnPhase.SetupPhase => GameAction.EndSetup
        case TurnPhase.MainPhase  => GameAction.EndTurn
    else
      val bestAction = ratedActions.max
      println(s"[BOT $name] Azione scelta: ${bestAction.action} (score: ${bestAction.score})")
      bestAction.action
