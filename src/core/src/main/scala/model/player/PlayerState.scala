package model.player 

import model.cards.*
import engine.* 

case class PlayerState(
  player: Player,
  territoryCards: Set[TerritoryCard] = Set.empty,
  objectiveCard: Option[ObjectiveCard] = None, 
  phase: TurnPhase = TurnPhase.SetupPhase, 
  bonusTroops: Int = 0
):
  def playerId: String = player.id
  
  def setObjectiveCard(objective: ObjectiveCard): PlayerState =
    copy(objectiveCard = Some(objective))
    
  def addTerritoryCard(card: TerritoryCard): PlayerState =
    copy(territoryCards = territoryCards + card)
    
  def addTerritoryCards(cards: Set[TerritoryCard]): PlayerState =
    copy(territoryCards = territoryCards ++ cards)
    
  def removeTerritoryCards(cards: Set[TerritoryCard]): PlayerState =
    copy(territoryCards = territoryCards -- cards)