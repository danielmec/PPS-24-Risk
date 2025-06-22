package strategy

import engine.GameState

trait StrategyRule:

    //return all possible actions for the player in the current game state
    def evaluateAction(gameState: GameState, playerId: String): Set[RatedAction]

object StrategyRule:
    extension(self: Set[StrategyRule])

        def evaluateAction(gameState: GameState, playerId: String): Set[RatedAction] =
            self.flatMap(_.evaluateAction(gameState, playerId))