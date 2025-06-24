package bot

import strategy._
import prolog._
import model.player.PlayerColor

object BotFactory:

  private val controller = new RiskBotController()

  def createAggressiveBot(playerId: String, name: String, color: PlayerColor): BotPlayer =
    val aggressiveRules = Set[StrategyRule](
      //aggiungere prolog rules
      new OffensiveBotAttackRule()
    )
    val botPlayer = new BotPlayer(playerId, name, color, aggressiveRules)
    controller.registerStrategy(playerId, botPlayer)
    botPlayer

  def createDefensiveBot(playerId: String, name: String, color: PlayerColor): BotPlayer =
    val defensiveRules = Set[StrategyRule](
      //aggiungere prolog rules
      new DefensiveBotAttackRule(),
      new DefensiveBotPlaceTroopsRule(),
      new DefensiveBotReinforceRule()
    )
    val botPlayer = new BotPlayer(playerId, name, color, defensiveRules)
    controller.registerStrategy(playerId, botPlayer)
    botPlayer