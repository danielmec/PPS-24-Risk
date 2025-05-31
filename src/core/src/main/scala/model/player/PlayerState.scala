package model.player 
import model.cards.*
import engine.* 

case class PlayerState(
    playerId: String, 
    territoryCards: Set[TerritoryCard], 
    objectiveCard: Option[ObjectiveCard], 
    phase: TurnPhase = TurnPhase.WaitingForTurn
):

    def setPhase(newPhase: TurnPhase): PlayerState = copy(phase = newPhase)

    def addTerritoryCard(card: TerritoryCard): PlayerState = copy(territoryCards = territoryCards + card)

    def removeTerritoryCards(cards: Set[TerritoryCard]): PlayerState = copy(territoryCards = territoryCards -- cards)

    def setObjectiveCard(card: ObjectiveCard): PlayerState = copy(objectiveCard = Some(card))

    def getObjectiveCard: Option[ObjectiveCard] = objectiveCard  