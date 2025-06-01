package engine
import model.cards.*
import exceptions.*

trait DecksManager:
    def shuffleTerritoriesDeck(): DecksManager
    def shuffleObjectivesDeck(): DecksManager
    def drawTerritory(): (DecksManager, TerritoryCard)
    def drawObjective(): (DecksManager, ObjectiveCard)
    def getRemainingTerritoriesCards: List[TerritoryCard]
    def getRemainingObjectiveCards: List[ObjectiveCard]

case class DecksManagerImpl(
    territories: List[TerritoryCard], 
    objectives: List[ObjectiveCard]) extends DecksManager:

    def shuffleTerritoriesDeck(): DecksManager = 
        copy(territories = scala.util.Random.shuffle(territories))
        
    def shuffleObjectivesDeck(): DecksManager = 
        copy(objectives = scala.util.Random.shuffle(objectives))

    private def draw[A](deck: List[A], exception: => Exception): (List[A], A) = deck match
        case h :: t => (t, h)
        case Nil => throw exception

    def drawTerritory(): (DecksManager, TerritoryCard) = 
        val (newDeck, territoryCard) = draw(territories, NoTerritoriesCardsException())
        (copy(territories = newDeck), territoryCard)

    def drawObjective(): (DecksManager, ObjectiveCard) = 
        val (newDeck, objectiveCard) = draw(objectives, NoObjectivesCardsException())
        (copy(objectives = newDeck), objectiveCard)

    def getRemainingTerritoriesCards: List[TerritoryCard] = territories
    
    def getRemainingObjectiveCards: List[ObjectiveCard] = objectives