package bot

import strategy.{Strategy, AggressiveStrategy, DefensiveStrategy}
import scala.util.Random

object BotFactory:
  private val random = new Random()

  //genera un ID casuale di 8 caratteri alfanumerici randomici
  private def generateId(): String =
    random.alphanumeric.take(8).mkString

  //determina la strategia iniziale in base al livello di difficoltà
  private def determineInitialStrategy(difficultyLevel: Int): Strategy =
    difficultyLevel match
      case 1 => new DefensiveStrategy()
      case 2 => new AggressiveStrategy()

  //crea un bot con configurazione casuale
  def createRandomBot(): BotPlayer =
    val difficultyLevel = random.nextInt(2) + 1  //genera il livello di difficoltà tra 1 e 2 randomicamente
    val strategy = determineInitialStrategy(difficultyLevel)
    val config = BotConfig(strategy, "RandomBot", difficultyLevel)
    createBot(config)

  //crea un bot con configurazione specifica
  def createBot(config: BotConfig): BotPlayer =
    val id = generateId()
    BotPlayer(
      id = id,
      currentStrategy = config.strategy,
      name = s"Bot-$id"
)
