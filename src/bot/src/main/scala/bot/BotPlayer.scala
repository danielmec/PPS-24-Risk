package bot

import strategy.Strategy
import bot.BotAction

case class BotPlayer(
  val id: String,
  val strategy: Strategy
):

  //decide la prossima mossa in base alla strategia che adotta
  //questo viene richiamato ad ogni fase durante tutto il turno del bot, quindi in fase di posizionamento
  //dirà che la nextmove è il PlaceTroops ed aggiorna lo stato, dopodichè viene richiamato e fa
  //eventualmente un attacco e cosi via

  //per farlo sarà da integrare con il core che si occupa della gestione del turno
  def nextMove(gameState: GameState): BotAction =
    strategy.decideMove(gameState)

