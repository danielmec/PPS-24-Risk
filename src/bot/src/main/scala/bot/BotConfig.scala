package bot

import strategy.Strategy
import scala.util.Random

/**
 * Configurazione per un bot
 * @param strategy la strategia che il bot utilizzerà per prendere decisioni
 * @param name il nome del bot
 * @param difficultyLevel livello di difficoltà del bot (1-2)
 */
case class BotConfig(
    strategy: Strategy,
    name: String,
    difficultyLevel: Int = 1
                    ):
  require(difficultyLevel == 1 || difficultyLevel == 2, "il livello di difficoltà deve essere 1 (difensivo) o 2 (aggressivo)")