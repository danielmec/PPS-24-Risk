package bot

import strategy._
import prolog._
import model.player.PlayerColor

object BotFactory:

  val controller = new RiskBotController()

  def createAggressiveBot(playerId: String, name: String, color: PlayerColor): (BotPlayer, RiskBotController) =
    val aggressiveRules = Set[StrategyRule](
      new BotSetupPlaceTroopsRule(),
      new OffensiveBotAttackRule(),
      new OffensiveBotPlaceTroopsRule(),
      new OffensiveBotReinforceRule()
    )
    val botPlayer = new BotPlayer(playerId, name, color, aggressiveRules)
    controller.registerStrategy(playerId, botPlayer)
    (botPlayer, controller)

  def createDefensiveBot(playerId: String, name: String, color: PlayerColor): (BotPlayer, RiskBotController) =
    val defensiveRules = Set[StrategyRule](
      new BotSetupPlaceTroopsRule(),
      new DefensiveBotAttackRule(),
      new DefensiveBotPlaceTroopsRule(),
      new DefensiveBotReinforceRule()
    )
    val botPlayer = new BotPlayer(playerId, name, color, defensiveRules)
    controller.registerStrategy(playerId, botPlayer)
    (botPlayer, controller)

  def getController(): RiskBotController = controller