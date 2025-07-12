package bot

import strategy._
import prolog._
import model.player.PlayerColor

/**
  * Factory object for creating bot players with predefined strategies.
  * Provides methods to create aggressive and defensive bots and to retrieve the shared controller.
  */
object BotFactory:

  private val controller = new RiskBotController()

  /**
    * Creates an aggressive bot player with the appropriate strategy rules.
    * Registers the bot's strategy with the controller.
    * @param playerId The unique identifier of the bot player.
    * @param name The name of the bot player.
    * @param color The color associated with the bot player.
    * @return A tuple containing the created BotPlayer and the RiskBotController.
    */
  def createAggressiveBot(playerId: String, name: String, color: PlayerColor): (BotPlayer, RiskBotController) =
    val aggressiveRules = Set[StrategyRule](
      new BotSetupPlaceTroopsRule(),
      new OffensiveBotPlaceTroopsRule(),
      new OffensiveBotAttackRule(),
      new OffensiveBotReinforceRule()
    )
    val botPlayer = new BotPlayer(playerId, name, color, aggressiveRules)
    controller.registerStrategy(playerId, botPlayer)
    (botPlayer, controller)

  /**
    * Creates a defensive bot player with the appropriate strategy rules.
    * Registers the bot's strategy with the controller.
    * @param playerId The unique identifier of the bot player.
    * @param name The name of the bot player.
    * @param color The color associated with the bot player.
    * @return A tuple containing the created BotPlayer and the RiskBotController.
    */
  def createDefensiveBot(playerId: String, name: String, color: PlayerColor): (BotPlayer, RiskBotController) =
    val defensiveRules = Set[StrategyRule](
      new BotSetupPlaceTroopsRule(),
      new DefensiveBotPlaceTroopsRule(),
      new DefensiveBotAttackRule(),
      new DefensiveBotReinforceRule()
    )
    val botPlayer = new BotPlayer(playerId, name, color, defensiveRules)
    controller.registerStrategy(playerId, botPlayer)
    (botPlayer, controller)

  /**
    * Retrieves the shared RiskBotController instance.
    * @return The RiskBotController used for managing bot strategies.
    */
  def getController(): RiskBotController = controller