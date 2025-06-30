package engine

import model.cards.*
import exceptions.*

trait DecksManager:
  def drawTerritory(): (DecksManager, TerritoryCard)
  def drawObjective(): (DecksManager, ObjectiveCard)
  def shuffleTerritoriesDeck(): DecksManager
  def shuffleObjectivesDeck(): DecksManager
  def territoriesCards: List[TerritoryCard]
  def objectiveCards: List[ObjectiveCard]

class DecksManagerImpl(
    private val territories: List[TerritoryCard],
    private val objectives: List[ObjectiveCard]
) extends DecksManager:
  def drawTerritory(): (DecksManager, TerritoryCard) =
    if territories.isEmpty then throw new NoTerritoriesCardsException()
    (DecksManagerImpl(territories.tail, objectives), territories.head)

  def drawObjective(): (DecksManager, ObjectiveCard) =
    if objectives.isEmpty then throw new NoObjectivesCardsException()
    (DecksManagerImpl(territories, objectives.tail), objectives.head)

  def shuffleTerritoriesDeck(): DecksManager =
    DecksManagerImpl(scala.util.Random.shuffle(territories), objectives)

  def shuffleObjectivesDeck(): DecksManager =
    DecksManagerImpl(territories, scala.util.Random.shuffle(objectives))

  def territoriesCards: List[TerritoryCard] = territories
  def objectiveCards: List[ObjectiveCard] = objectives