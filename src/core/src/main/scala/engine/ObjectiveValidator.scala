package engine

import model.cards.*
import model.player.*
import model.board.*

object ObjectiveValidator:

  def done(objective: ObjectiveCard, gameState: GameState, playerState: PlayerState): Boolean =
    objective match
      case ObjectiveCard.ConquerTerritories(num, minTroopsToOwn) =>
        val owned = gameState.board.territories.count: territory =>
          territory.owner.exists(_.id == playerState.playerId) && territory.troops >= minTroopsToOwn
        owned >= num

      case ObjectiveCard.ConquerContinents(continents) =>
        continents.forall: continent =>
          gameState.board.continents.find(_.name == continent.name)
            .exists(_.isFullyOwnedBy(playerState.playerId))

      case ObjectiveCard.ConquerNContinents(n) =>
        val ownedContinents = gameState.board.continents.count: continent =>
          continent.isFullyOwnedBy(playerState.playerId)
        ownedContinents >= n

      case null => false