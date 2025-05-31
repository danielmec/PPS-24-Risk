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