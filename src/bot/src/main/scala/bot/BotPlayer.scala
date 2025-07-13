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

/**
  * Represents a bot player in the game.
  * A BotPlayer uses a set of strategy rules to decide its moves automatically.
  *
  * @param id The unique identifier of the bot player.
  * @param name The name of the bot player.
  * @param color The color associated with the bot player.
  * @param strategyRules The set of strategy rules that define the bot's behavior.
  */
class BotPlayer(
    override val id: String,
    override val name: String,
    override val color: PlayerColor,
    val strategyRules: Set[StrategyRule]
) extends Player, Strategy:

  /**
    * The type of player (always Bot for BotPlayer).
    * @return PlayerType.Bot
    */
  override def playerType: PlayerType = PlayerType.Bot
  
  private val isOffensive: Boolean = 
    strategyRules.exists(_.isInstanceOf[OffensiveBotAttackRule])
  
  /**
   * Decides the next move based on the current game state and the bot's strategy rules.
   * Evaluates the available actions according to the rules defined in `strategyRules`.
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
            println(s"[BOT DEBUG] Possible attacks for $name:")
            attackActions.foreach { ra =>
              println(s" Action: ${ra.action}, Score: ${ra.score}, Explanation: ${ra.explaination}")
            }
            val bestAttack = attackActions.max
            println(s"[BOT DEBUG] Chosen attack for $name: ${bestAttack.action} (Score: ${bestAttack.score})")
            bestAttack.action match
              case _: GameAction.Attack => Thread.sleep(3000)
              case _ =>
            return bestAttack.action
          
          val reinforceRules = 
            if (isOffensive) then strategyRules.filter(_.isInstanceOf[OffensiveBotReinforceRule])
            else strategyRules.filter(_.isInstanceOf[DefensiveBotReinforceRule])
          val reinforceActions = reinforceRules.evaluateAction(gameState, id)
          
          if reinforceActions.nonEmpty then
            println(s"[BOT DEBUG] Possible reinforces for $name:")
            reinforceActions.foreach { ra =>
              println(s" Action: ${ra.action}, Score: ${ra.score}, Explanation: ${ra.explaination}")
            }
            val bestReinforce = reinforceActions.max
            println(s"[BOT DEBUG] Chosen reinforce for $name: ${bestReinforce.action} (Score: ${bestReinforce.score})")
            return bestReinforce.action
          
          return GameAction.EndTurn
  
    val ratedActions = phaseAppropriateRules.evaluateAction(gameState, id)
    if ratedActions.isEmpty then
      gameState.turnManager.currentPhase match
        case TurnPhase.SetupPhase => GameAction.EndSetup
        case TurnPhase.MainPhase  => GameAction.EndTurn
    else
      val bestAction = ratedActions.max
      bestAction.action