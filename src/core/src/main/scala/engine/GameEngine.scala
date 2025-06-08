package engine

import model.player.*
import model.cards.*
import model.board.*

class GameEngine(
    val players: List[PlayerImpl],
    val gameId: String = java.util.UUID.randomUUID().toString
):
  private val (continents, territoriesMap) = CardsBuilder.createBoard()
  private val board = Board(gameId, continents)
  private val territoryDeck = CardsBuilder.createTerritoriesDeck()
  private val objectiveDeck = CardsBuilder.createObjectivesDeck()

  private val playerStates: List[PlayerState] = players.map: p =>
    PlayerState(p, Set.empty, None)

  private var turnManager: TurnManager = TurnManagerImpl(players)
  private var decksManager: DecksManager = DecksManagerImpl(territoryDeck, objectiveDeck)

  private var gameState: GameState = GameState(
    gameId = gameId,
    board = board,
    playerStates = playerStates,
    turnManager = turnManager,
    decksManager = decksManager,
    territoryCards = territoryDeck,
    objectiveCards = objectiveDeck
  )

  /** Esegue un'azione di gioco e aggiorna lo stato */
  def performAction(action: GameAction): Either[String, GameState] = 
    if (!gameState.turnManager.isValidAction(action))
      Left("Azione non valida per la fase o il giocatore corrente.")
    else 
      action match
        case GameAction.PlaceTroops(playerId, troops, territoryName) =>
          val maybeTerritory = gameState.board.territories.find(_.name == territoryName)
          maybeTerritory match
            case Some(territory) if territory.owner.exists(_.id == playerId) =>
              val updatedTerritory = territory.copy(troops = territory.troops + troops)
              val updatedBoard = gameState.board.updatedTerritory(updatedTerritory)
              gameState = gameState.updateBoard(updatedBoard)
              Right(gameState)
            case _ =>
              Left("Territorio non valido o non posseduto dal giocatore.")

        case GameAction.Reinforce(playerId, troops) =>
          Right(gameState) // TODO

        case GameAction.Attack(attackerId, defenderId) =>
          Right(gameState) // TODO

        case GameAction.Defend(defenderId, troops) =>
          Right(gameState) // TODO

        case GameAction.TradeCards(cards) =>
          Right(gameState) // TODO

        case GameAction.EndAttack | GameAction.EndPhase | GameAction.EndTurn =>
          turnManager = turnManager.nextPhase()
          gameState = gameState.updateTurnManager(turnManager)
          Right(gameState)

  def getGameState: GameState = gameState

  def checkVictory: Option[PlayerState] =
    gameState.checkWinCondition

