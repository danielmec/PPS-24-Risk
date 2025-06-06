package bot

import strategy.Strategy
import bot.BotAction

case class BotPlayer(
                      val id: String,
                      val strategy: Strategy,
                      val name: String = s"Bot-$id"
                    ):

  val strategyManager = new StrategyManager() //crea il manager delle strategie

  //decide la prossima mossa in base alla strategia che adotta
  //questo viene richiamato ad ogni fase durante tutto il turno del bot, quindi in fase di posizionamento
  //dirà che la nextmove è il PlaceTroops ed aggiorna lo stato, dopodichè viene richiamato e fa
  //eventualmente un attacco e cosi via

  //per farlo sarà da integrare con il core che si occupa della gestione del turno
  def nextMove(gameState: GameState): BotAction =
    updateStrategyIfNeeded(gameState)
    strategy.decideMove(gameState)

  //da aggiungere solo se vogliamo fare che la strategy sia modificabile in base allo stato del gioco
  def updateStrategyIfNeeded(gameState: GameState): Unit =
    val newStrategy = strategyManager.evaluateStrategy(gameState)
    if (newStrategy.getClass != strategy.getClass)
      strategy = newStrategy
      println(s"$name changed strategy to ${newStrategy.getClass.getSimpleName}")

  def changeStrategy(newStrategy: Strategy): Unit =
    strategy = newStrategy
