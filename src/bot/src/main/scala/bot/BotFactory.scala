package bot

import strategy.Strategy
import model.player.PlayerColor

object BotFactory:

  private val controller = new RiskBotController()

  def createAggressiveBot(playerId: String, color: PlayerColor): BotPlayer =
    val aggressiveRules = Set[StrategyRule](
      new PrologRule("prologFileName")
    )
    controller.registerStrategy(playerId, new BotPlayer(playerId, aggressiveRules))

  def createDefensiveBot(playerId: String, color: PlayerColor): BotPlayer =
    val defensiveRules = Set[StrategyRule](
      //aggiungere prolog rules
      new PrologRule("prologFileName")
    )
    controller.registerStrategy(playerId, new BotPlayer(playerId, defensiveRules))

