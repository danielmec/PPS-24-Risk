case class Continent(
  name: String, 
  territories: Set[Territory], 
  bonusTroops: Int
):

  def isFullyOwnedBy(player: Player): Boolean =
    territories.forall(_.owner.contains(player))

  def territoriesOf(player: Player): Set[Territory] =
    territories.filter(_.owner.contains(player))