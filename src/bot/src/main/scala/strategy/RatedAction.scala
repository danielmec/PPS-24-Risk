package strategy

import engine.GameAction

case class RatedAction(
    action: GameAction,
    score: Double,
    explaination: String
)

object RatedAction:
    //sort the actions by score
    given Ordering[RatedAction] = (x, y) => y.score.compareTo(x.score)
