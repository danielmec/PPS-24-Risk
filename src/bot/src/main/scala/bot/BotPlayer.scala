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
  
  // Determina il tipo di strategia in base alle regole disponibili
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
        // In fase di setup, usa solo le regole di setup
        val setupRules = strategyRules.filter(_.isInstanceOf[BotSetupPlaceTroopsRule])
        println(s"[BOT $name] Fase di SETUP: utilizzando ${setupRules.size} regole di setup")
        println("strategyRules: " + strategyRules.map(_.getClass.getSimpleName).mkString(", "))
       
        setupRules
        
      case TurnPhase.MainPhase => 
        if currentPlayerState.bonusTroops > 0 then

          val placeTroopsRules = if (isOffensive)
            strategyRules.filter(_.isInstanceOf[OffensiveBotPlaceTroopsRule])
          else
            strategyRules.filter(_.isInstanceOf[DefensiveBotPlaceTroopsRule])
            
          println(s"[BOT $name] MAIN PHASE - PIAZZAMENTO: utilizzando ${placeTroopsRules.size} regole di piazzamento ${if (isOffensive) "offensive" else "difensive"}")
          placeTroopsRules
        else
         
          val attackRules = if (isOffensive)
            strategyRules.filter(_.isInstanceOf[OffensiveBotAttackRule])
          else
            strategyRules.filter(_.isInstanceOf[DefensiveBotAttackRule])
            
          println(s"[BOT $name] MAIN PHASE - Controllo attacchi: utilizzando ${attackRules.size} regole di attacco ${if (isOffensive) "offensive" else "difensive"}")
          val attackActions = attackRules.evaluateAction(gameState, id)
          
          if attackActions.nonEmpty then
            val bestAttack = attackActions.max
            println(s"[BOT $name] Trovati ${attackActions.size} attacchi validi, eseguo: ${bestAttack.action}")
            
            bestAttack.action match
              case _: GameAction.Attack =>
                println(s"[BOT $name] Sto pensando al prossimo attacco...")
                Thread.sleep(3000)
                println(s"[BOT $name] Attacco deciso!")
              case _ =>
            
            return bestAttack.action
          
          // Usa le regole di rinforzo appropriate
          println(s"[BOT $name] Nessun attacco valido disponibile, passo ai rinforzi")
          val reinforceRules = if (isOffensive)
            strategyRules.filter(_.isInstanceOf[OffensiveBotReinforceRule])
          else
            strategyRules.filter(_.isInstanceOf[DefensiveBotReinforceRule])
            
          println(s"[BOT $name] MAIN PHASE - Controllo rinforzi: utilizzando ${reinforceRules.size} regole di rinforzo ${if (isOffensive) "offensive" else "difensive"}")
          val reinforceActions = reinforceRules.evaluateAction(gameState, id)
          
          if reinforceActions.nonEmpty then
            println(s"[BOT $name] Trovati ${reinforceActions.size} rinforzi validi, scelgo il migliore")
            return reinforceActions.max.action
          
          // 3 termina il turno
          println(s"[BOT $name] Nessuna azione valida, termino il turno")
          return GameAction.EndTurn
  
    // Esegue la regola appropriata alla fase corrente con PrologRule
    val ratedActions = phaseAppropriateRules.evaluateAction(gameState, id)
    
    if ratedActions.isEmpty then
      gameState.turnManager.currentPhase match
        case TurnPhase.SetupPhase => GameAction.EndSetup
        case TurnPhase.MainPhase  => GameAction.EndTurn
    else
      val bestAction = ratedActions.max
      println(s"[BOT $name] Azione scelta: ${bestAction.action} (score: ${bestAction.score})")
      bestAction.action
