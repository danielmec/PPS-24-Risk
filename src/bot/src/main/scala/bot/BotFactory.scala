package bot

import strategy.Strategy

object BotFactory:
    def createAggressiveBot(playerId: String): Strategy =
        new BotPlayer(playerId, Set())

    def createDefensiveBot(playerId: String): Strategy =
        new BotPlayer(playerId, Set())
