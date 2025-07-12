package strategy

import engine.GameAction

/**
  * Represents an action with an associated score and explanation.
  * Used to rate and compare possible actions for a bot.
  *
  * @param action The GameAction to be performed.
  * @param score The score assigned to the action.
  * @param explaination A textual explanation of the action.
  */
case class RatedAction(
    action: GameAction,
    score: Double,
    explaination: String
)

/**
  * Companion object for RatedAction.
  * Provides an ordering to sort actions by score (descending).
  */
object RatedAction:
    /**
      * Ordering for RatedAction by descending score.
      */
    given Ordering[RatedAction] = (x, y) => y.score.compareTo(x.score)
