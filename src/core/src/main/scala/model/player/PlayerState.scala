package model.player 

import model.cards.*
import engine.* 

/**
  * Represents the state of a player during the game.
  *
  * @param player The player associated with this state.
  * @param territoryCards The set of territory cards owned by the player.
  * @param objectiveCard The objective card assigned to the player, if any.
  * @param phase The current turn phase for the player.
  * @param bonusTroops The number of bonus troops available to the player.
  */
case class PlayerState(
  player: Player,
  territoryCards: Set[TerritoryCard] = Set.empty,
  objectiveCard: Option[ObjectiveCard] = None, 
  phase: TurnPhase = TurnPhase.SetupPhase, 
  bonusTroops: Int = 0
):
  /**
    * Returns the unique identifier of the player.
    */
  def playerId: String = player.id
  
  /**
    * Sets the objective card for the player.
    * @param objective The objective card to assign.
    * @return The updated PlayerState.
    */
  def setObjectiveCard(objective: ObjectiveCard): PlayerState =
    copy(objectiveCard = Some(objective))
    
  /**
    * Adds a territory card to the player's set.
    * @param card The territory card to add.
    * @return The updated PlayerState.
    */
  def addTerritoryCard(card: TerritoryCard): PlayerState =
    copy(territoryCards = territoryCards + card)
    
  /**
    * Adds multiple territory cards to the player's set.
    * @param cards The set of territory cards to add.
    * @return The updated PlayerState.
    */
  def addTerritoryCards(cards: Set[TerritoryCard]): PlayerState =
    copy(territoryCards = territoryCards ++ cards)
    
  /**
    * Removes a set of territory cards from the player's set.
    * @param cards The set of territory cards to remove.
    * @return The updated PlayerState.
    */
  def removeTerritoryCards(cards: Set[TerritoryCard]): PlayerState =
    copy(territoryCards = territoryCards -- cards)