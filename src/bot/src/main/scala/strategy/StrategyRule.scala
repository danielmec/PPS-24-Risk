package strategy

import engine.GameState

/**
  * Trait for a single strategy rule.
  * A StrategyRule evaluates all possible actions for a player in the current game state.
  */
trait StrategyRule:

    /**
      * Returns all possible actions for the player in the current game state.
      * @param gameState The current state of the game.
      * @param playerId The id of the player for whom to evaluate actions.
      * @return A set of RatedAction representing possible actions and their scores.
      */
    def evaluateAction(gameState: GameState, playerId: String): Set[RatedAction]

/**
  * Companion object for StrategyRule.
  * Provides extension methods for sets of StrategyRule.
  */
object StrategyRule:
    extension(self: Set[StrategyRule])

        /**
          * Evaluates all possible actions for all rules in the set and combines the results.
          * @param gameState The current state of the game.
          * @param playerId The id of the player for whom to evaluate actions.
          * @return A set of RatedAction from all rules.
          */
        def evaluateAction(gameState: GameState, playerId: String): Set[RatedAction] =
            self.flatMap(_.evaluateAction(gameState, playerId)) 