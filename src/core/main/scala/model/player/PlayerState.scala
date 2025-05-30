import engine.TurnPhase

case class PlayerState(
    playerId: String, 
    cards: Set[Card], 
    objective: Set[Objective], 
    phase: TurnPhase = TurnPhase.WaitingForTurn
):

    def setPhase(newPhase: TurnPhase): PlayerState = copy(phase = newPhase)

    def addCard(card: Card): PlayerState = copy(cards = cards + card)

    def removeCards(cardsToRemove: Set[Card]): PlayerState = copy(cards = cards -- cardsToRemove)

    






  