package bot

import strategy.{Strategy, RandomStrategy, AggressiveStrategy, DefensiveStrategy}
import scala.util.Random

//crea diversi bot con diverse configurazioni
object BotFactory:
  val random = new Random()

  //genera un ID casuale di 8 caratteri esadecimali per decidere il nome del bot
  private def generateId(): String =
    random.alphanumeric.take(8).mkString

  //crea il bot in base alla configurazione passata
  def createBot(config: BotConfig): BotPlayer =
    BotPlayer(
      id = generateId(),
      currentStrategy = config.strategy,
      name = config.name
    )

  //metodo di prova con la random strategy (da cancellare insieme alla random strategy)
  def createRandomBot(): BotPlayer =
    val config = BotConfig.randomConfig(
      strategy = new RandomStrategy(),
      name = "RandomBot"
    )
    createBot(config)

  //crea un bot con strategia aggressiva
  def createAggressiveBot(): BotPlayer =
    val config = BotConfig(
      strategy = new AggressiveStrategy(),
      name = "AggressiveBot",
      difficultyLevel = 2,
      aggressiveness = 0.8
    )
    createBot(config)


  //crea un bot con strategia difensiva
  def createDefensiveBot(): BotPlayer =
    val config = BotConfig(
      strategy = new DefensiveStrategy(),
      name = "DefensiveBot",
      difficultyLevel = 2,
      aggressiveness = 0.2
    )
    createBot(config)

