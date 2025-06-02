package model.board
import model.player.Player
import model.player.PlayerState

case class Territory(
  name: String, 
  neighbors: Set[Territory], 
  owner: Option[Player], 
  troops: Int
):

  def hasEnoughTroops(requiredTroops: Int): Boolean = troops >= requiredTroops

  def addTroops(newTroop: Int): Territory = 
    copy(troops = troops + newTroop)

  def removeTroops(troopsToRemove: Int): Territory = 
    copy(troops = (troops - troopsToRemove).max(0))

  def isOwnedBy(playerId: String): Boolean = 
    owner.exists(_.id == playerId)

  def changeOwner(newOwner: Player): Territory = 
    copy(owner = Some(newOwner))

  // Added to avoid infinite loops 
  override def toString(): String = 
    s"Territory($name, ${neighbors.size} neighbors, $owner, $troops)"
  
  override def equals(obj: Any): Boolean = obj match
    case t: Territory => t.name == this.name
    case _ => false
  
  override def hashCode(): Int = name.hashCode