package bot

import strategy.Strategy
import strategy.StrategyManager
import strategy.BotAction
import engine.GameState

/**
 * Rappresenta un giocatore bot controllato dall'IA.
 * @param id identificativo univoco del bot
 * @param config configurazione attuale del bot, contiene nome, strategia e livello di difficoltà
 */
case class BotPlayer(
  val id: String,
  var config: BotConfig // contiene già nome, strategy, difficultyLevel
):

  // Manager che valuta se cambiare strategia in base allo stato del gioco
  val strategyManager = new StrategyManager(config)

  /**
   * Decide la prossima mossa del bot in base alla strategia attuale.
   * Questo metodo viene richiamato dal core ad ogni fase del turno del bot.
   * @param gameState lo stato attuale della partita
   * @return la prossima azione da eseguire (BotAction)
   */
  def nextMove(gameState: GameState): BotAction =
    updateStrategyIfNeeded(gameState)
    config.strategy.decideMove(gameState)

  /**
   * Aggiorna la strategia del bot se necessario, in base allo stato del gioco.
   * Può essere esteso per rendere il bot più adattivo.
   */
  def updateStrategyIfNeeded(gameState: GameState): Unit =
    val maybeNewStrategy = strategyManager.evaluateStrategy(gameState, config.strategy)
    maybeNewStrategy.foreach { newStrategy =>
      config = config.copy(strategy = newStrategy)
      println(s"${config.name} changed strategy to ${newStrategy.getClass.getSimpleName}")
  }

  /**
   * Cambia manualmente la strategia del bot.
   * @param newStrategy la nuova strategia da adottare
   */
  def changeStrategy(newStrategy: Strategy): Unit =
    config = config.copy(strategy = newStrategy)
