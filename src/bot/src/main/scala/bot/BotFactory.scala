package bot

import strategy.{Strategy, RandomStrategy, AggressiveStrategy, DefensiveStrategy}
import java.util.UUID

/**
 * Factory per la creazione di diversi tipi di bot
 */
object BotFactory:
  /**
   * Crea un bot con una configurazione specifica
   * @param config configurazione del bot
   * @return una nuova istanza di BotPlayer
   */
  def createBot(config: BotConfig): BotPlayer = 
    BotPlayer(
      id = UUID.randomUUID().toString,
      currentStrategy = config.strategy,
      name = config.name
    )
  
  /**
   * Crea un bot con strategia random
   * @return una nuova istanza di BotPlayer con strategia random
   */
  def createRandomBot(): BotPlayer =
    val config = BotConfig.randomConfig(
      strategy = new RandomStrategy(),
      name = "RandomBot"
    )
    createBot(config)
  
  /**
   * Crea un bot con strategia aggressiva
   * @return una nuova istanza di BotPlayer con strategia aggressiva
   */
  def createAggressiveBot(): BotPlayer =
    val config = BotConfig(
      strategy = new AggressiveStrategy(),
      name = "AggressiveBot",
      difficultyLevel = 2,
      aggressiveness = 0.8
    )
    createBot(config)
  
  /**
   * Crea un bot con strategia difensiva
   * @return una nuova istanza di BotPlayer con strategia difensiva
   */
  def createDefensiveBot(): BotPlayer =
    val config = BotConfig(
      strategy = new DefensiveStrategy(),
      name = "DefensiveBot",
      difficultyLevel = 2,
      aggressiveness = 0.2
    )
    createBot(config)

