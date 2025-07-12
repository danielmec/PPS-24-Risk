package model.board

import model.player.*

case class Continent(
  name: String, 
  territories: Set[Territory], 
  bonusTroops: Int
):

  def isFullyOwnedBy(playerId: String): Boolean = territories.forall(_.owner.exists(_.id == playerId))