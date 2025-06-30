package model.board

import model.player.*

case class Continent(
  name: String, 
  territories: Set[Territory], 
  bonusTroops: Int
):

  def isFullyOwnedBy(player: PlayerState): Boolean =
    territories.forall(_.owner.contains(player))

  def territoriesOf(player: PlayerState): Set[Territory] =
    territories.filter(_.owner.contains(player))