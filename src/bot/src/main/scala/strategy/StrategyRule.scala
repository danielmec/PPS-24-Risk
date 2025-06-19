package strategy

import engine.GameState

trait StrategyRule:

    //return all possible actions for the player in the current game state
    def evaluateAction(gameState: GameState, playerId: String): Set[RatedAction]
/**
 * Trait che definisce una regola di strategia per il bot
 */

object StrategyRule:
    extension(self: Set[StrategyRule])

    /**
     * Valuta le azioni possibili per il giocatore in base allo stato del gioco
     * @param gamestate lo stato corrente del gioco
     * @param playerId l'ID del giocatore per cui valutare le azioni
     * @return un set di azioni possibili con la loro valutazione
     */
        def evaluateAction(gameState: GameState, playerId: String): Set[RatedAction] =
            self.flatMap(_.evaluateAction(gameState, playerId))