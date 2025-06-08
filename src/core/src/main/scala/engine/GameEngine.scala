package engine

import model.player.*
import model.cards.*
import model.board.*
import exceptions._

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

        case GameAction.Attack(attackerId, defenderId, from, to, troops) =>
          val maybeAttacker = players.find(_.id == attackerId)
          val maybeDefender = players.find(_.id == defenderId)
          val maybeAttackerTerritory = gameState.board.territories.find(_.name == from)
          val maybeDefenderTerritory = gameState.board.territories.find(_.name == to)

          (maybeAttacker, maybeDefender, maybeAttackerTerritory, maybeDefenderTerritory) match
            case (Some(attacker), Some(defender), Some(attackerTerritory), Some(defenderTerritory)) =>
              try
                val (result, updatedAttackerTerritory, updatedDefenderTerritory) =
                  Battle.battle(attacker, defender, attackerTerritory, defenderTerritory, troops)
                val updatedBoard = gameState.board
                  .updatedTerritory(updatedAttackerTerritory)
                  .updatedTerritory(updatedDefenderTerritory)
                gameState = gameState.updateBoard(updatedBoard)
                Right(gameState)
              catch
                case _: InvalidAttackException =>
                  Left("Attacco non valido: controlla i territori, i proprietari o il numero di truppe.")
            case _ =>
              Left("Territori o giocatori non validi per l'attacco.")

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

