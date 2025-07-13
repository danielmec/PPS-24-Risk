package engine

import model.cards.*
import exceptions.*

/**
  * Trait for managing the decks of territory and objective cards.
  */
trait DecksManager:
  /**
    * Draws a territory card from the deck.
    * @return A tuple of the updated DecksManager and the drawn TerritoryCard.
    */
  def drawTerritory(): (DecksManager, TerritoryCard)
  /**
    * Draws an objective card from the deck.
    * @return A tuple of the updated DecksManager and the drawn ObjectiveCard.
    */
  def drawObjective(): (DecksManager, ObjectiveCard)
  /**
    * Shuffles the territory deck.
    * @return The updated DecksManager.
    */
  def shuffleTerritoriesDeck(): DecksManager
  /**
    * Shuffles the objective deck.
    * @return The updated DecksManager.
    */
  def shuffleObjectivesDeck(): DecksManager
  /**
    * Returns the list of territory cards in the deck.
    */
  def territoriesCards: List[TerritoryCard]
  /**
    * Returns the list of objective cards in the deck.
    */
  def objectiveCards: List[ObjectiveCard]

/**
  * Implementation of DecksManager using immutable lists.
  *
  * @param territories The list of territory cards.
  * @param objectives The list of objective cards.
  */
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