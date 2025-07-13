package engine

import model.cards.*
import model.player.*
import model.board.*

/**
  * Utility object for validating if a player has completed their objective.
  */
object ObjectiveValidator:

  /**
    * Checks if the given objective is completed by the player in the current game state.
    *
    * @param objective The objective card to check.
    * @param gameState The current state of the game, including board information.
    * @param playerState The current state of the player.
    * @return True if the objective is completed, false otherwise.
    */
  def done(objective: Option[ObjectiveCard], gameState: GameState, playerState: PlayerState): Boolean =
    objective.exists(card => evaluateObjective(card, gameState, playerState))
    
  /**
    * Evaluates whether the given objective is satisfied by the player.
    *
    * @param objective The objective to evaluate.
    * @param gameState The current state of the game.
    * @param playerState The state of the player being evaluated.
    * @return True if the objective conditions are met, false otherwise.
    */
  private def evaluateObjective(objective: ObjectiveCard, gameState: GameState, playerState: PlayerState): Boolean =
    objective match
      /**
        * Checks if the player owns at least the specified number of territories,
        * each with a minimum number of troops.
        */
      case ObjectiveCard.ConquerTerritories(num, minTroopsToOwn) =>
        val owned = gameState.board.territories.count: territory =>
          territory.owner.exists(_.id == playerState.playerId) && territory.troops >= minTroopsToOwn
        owned >= num

      /**
        * Checks if the player fully owns all the continents listed in the objective.
        */
      case ObjectiveCard.ConquerContinents(continents) =>
        continents.forall: continent =>
          gameState.board.continents.find(_.name == continent.name)
            .exists(_.isFullyOwnedBy(playerState.playerId))

      /**
        * Checks if the player fully owns at least the specified number of any continents.
        */
      case ObjectiveCard.ConquerNContinents(n) =>
        val ownedContinents = gameState.board.continents.count: continent =>
          continent.isFullyOwnedBy(playerState.playerId)
        ownedContinents >= n
