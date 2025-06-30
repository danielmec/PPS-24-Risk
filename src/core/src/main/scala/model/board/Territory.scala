package model.board

import model.player.Player
import model.player.PlayerState

case class Territory(
  name: String,
  owner: Option[Player] = None,
  troops: Int = 0,
  neighbors: Set[Territory] = Set.empty
):
  def isOwnedBy(playerId: String): Boolean = 
    owner.exists(_.id == playerId)
    
  def addTroops(amount: Int): Territory =
    copy(troops = troops + amount)
    
  def removeTroops(amount: Int): Territory =
    copy(troops = troops - amount)
    
  def hasEnoughTroops(minimumRequired: Int): Boolean =
    troops >= minimumRequired

  def changeOwner(newOwner: Player): Territory =
    copy(owner = Some(newOwner))
    
  def canAttack(target: Territory): Boolean =
    hasEnoughTroops(2) && 
    neighbors.exists(_.name == target.name) &&
    owner.isDefined && target.owner.isDefined &&
    owner.get.id != target.owner.get.id

  
  override def toString(): String = 
    s"Territory($name, ${neighbors.size} neighbors, $owner, $troops)"
  
  override def equals(obj: Any): Boolean = obj match
    case t: Territory => t.name == this.name
    case _ => false
  
  override def hashCode(): Int = name.hashCode