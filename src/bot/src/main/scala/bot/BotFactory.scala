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
    val id = generateId()
    val config = BotConfig(strategy, s"Bot-$id", difficultyLevel)
    BotPlayer(
      id = id,
      config = config
    )

  //crea un bot con configurazione specifica
  def createBot(config: BotConfig): BotPlayer =
    val id = generateId()
    // aggiorna il nome nel config per coerenza con l'id generato
    val configWithName = config.copy(name = s"Bot-$id")
    BotPlayer(
      id = id,
      config = configWithName
    )