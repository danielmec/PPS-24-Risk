package bot.strategy

import bot.BotAction

class RandomStrategy extends Strategy {
  override def decideMove(gameState: Any): BotAction = {
    //Azione fissa di prova: piazza 1 truppa nel territorio "Territory1"
    BotAction.PlaceTroops("Territory1", 1)
  }
}
