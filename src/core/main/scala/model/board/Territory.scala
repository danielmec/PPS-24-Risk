case class Territory(
  name: String, 
  neighbors: Set[Territories], 
  owner: Option[PlayerState], 
  troops: Int
):

  def addTroops(newTroop: Int): Territory = 
    copy(troops = troops + newTroop)

  def removeTroops(troopsToRemove: Int): Territory = 
    copy(troops = (troops - troopsToRemove).max(0))

  def changeOwner(newOwner: PlayerState): Territory = 
    copy(owner = Some(newOwner))