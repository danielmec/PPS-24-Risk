case class Territory(
  name: String, 
  neighbors: Set[String], 
  owner: Option[Player], 
  troops: Int
):

  def addTroops(newTroop: Int): Territory = 
    copy(troops = troops + newTroop)

  def removeTroops(troopsToRemove: Int): Territory = 
    copy(troops = (troops - troopsToRemove).max(0))

  def changeOwner(newOwner: Player): Territory = 
    copy(owner = Some(newOwner))