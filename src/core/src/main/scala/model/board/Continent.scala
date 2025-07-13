package model.board

import model.player.*

/**
  * Represents a continent on the game board.
  *
  * @param name The name of the continent.
  * @param territories The set of territories in the continent.
  * @param bonusTroops The bonus troops awarded for controlling the continent.
  */
case class Continent(
  name: String, 
  territories: Set[Territory], 
  bonusTroops: Int
):

  /**
    * Checks if the continent is fully owned by the player with the given ID.
    * @param playerId The ID of the player.
    * @return True if all territories are owned by the player, false otherwise.
    */
  def isFullyOwnedBy(playerId: String): Boolean = territories.forall(_.owner.exists(_.id == playerId))