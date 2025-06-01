package engine

import model.cards.*
import model.player.*
import model.board.*

object ObjectiveValidator:

  def isCompleted(objective: ObjectiveCard, gameState: GameState, playerState: PlayerState): Boolean =
    objective match
      case ObjectiveCard.ConquerTerritories(num, minTroops) =>
        // Conta i territori posseduti con almeno minTroops truppe
        val owned = gameState.board.territories.count: t =>
          t.owner.exists(_.id == playerState.playerId) && t.troops >= minTroops
        
        owned >= num

      case ObjectiveCard.ConquerContinents(continents) =>
        // Verifica se il giocatore possiede tutti i territori di ciascun continente richiesto
        continents.forall: continent =>
          continent.territories.forall: t =>
            gameState.board.territories.find(_.name == t.name).exists(_.owner.exists(_.id == playerState.playerId))
        

      case ObjectiveCard.EliminatePlayer(targetColor) =>
        // Verifica se il giocatore target non possiede più territori
        !gameState.board.territories.exists(_.owner.exists(_.color == targetColor))

      case ObjectiveCard.ConquerNContinents(n) =>
        // Conta i continenti completamente posseduti
        val ownedContinents = gameState.board.continents.count: continent =>
          continent.territories.forall: t =>
            gameState.board.territories.find(_.name == t.name).exists(_.owner.exists(_.id == playerState.playerId))
        ownedContinents >= n

      case _ => false

