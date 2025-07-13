package model.board

import model.player.Player
import model.player.PlayerState

/**
  * Represents a territory on the game board.
  *
  * @param name The name of the territory.
  * @param owner The player who owns the territory, if any.
  * @param troops The number of troops stationed in the territory.
  * @param neighbors The set of neighboring territories.
  */
case class Territory(
  name: String,
  owner: Option[Player] = None,
  troops: Int = 0,
  neighbors: Set[Territory] = Set.empty
):
  /**
    * Checks if the territory is owned by the player with the given ID.
    * @param playerId The ID of the player.
    * @return True if the territory is owned by the player, false otherwise.
    */
  def isOwnedBy(playerId: String): Boolean = 
    owner.exists(_.id == playerId)
    
  /**
    * Adds troops to the territory.
    * @param amount The number of troops to add.
    * @return The updated Territory.
    */
  def addTroops(amount: Int): Territory =
    copy(troops = troops + amount)
    
  /**
    * Removes troops from the territory.
    * @param amount The number of troops to remove.
    * @return The updated Territory.
    */
  def removeTroops(amount: Int): Territory =
    copy(troops = troops - amount)
    
  /**
    * Checks if the territory has at least the given number of troops.
    * @param minimumRequired The minimum number of troops required.
    * @return True if the territory has enough troops, false otherwise.
    */
  def hasEnoughTroops(minimumRequired: Int): Boolean =
    troops >= minimumRequired

  /**
    * Changes the owner of the territory.
    * @param newOwner The new owner of the territory.
    * @return The updated Territory.
    */
  def changeOwner(newOwner: Player): Territory =
    copy(owner = Some(newOwner))
  
  override def toString(): String = 
    s"Territory($name, ${neighbors.size} neighbors, $owner, $troops)"
  
  override def equals(obj: Any): Boolean = obj match
    case t: Territory => t.name == this.name
    case _ => false
  
  override def hashCode(): Int = name.hashCode