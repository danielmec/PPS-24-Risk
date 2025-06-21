package core.engine

import org.scalatest.funsuite.AnyFunSuite
import model.cards.*
import model.player.*
import model.board.*
import engine.*

class GameStateTest extends AnyFunSuite:

  val player = PlayerImpl("1", "Alice", PlayerColor.Red, PlayerType.Human)
  val playerState = PlayerState(player, Set.empty, None, TurnPhase.WaitingForTurn, 0)
  val otherPlayer = PlayerImpl("2", "Bob", PlayerColor.Blue, PlayerType.Human)
  val otherState = PlayerState(otherPlayer, Set.empty, None, TurnPhase.WaitingForTurn, 0)

  val territory1 = Territory("T1", Some(player), 3, Set.empty)
  val territory2 = Territory("T2", Some(player), 2, Set.empty)
  val territory3 = Territory("T3", Some(otherPlayer), 1, Set.empty)
  val continent = Continent("Europe", Set(territory1, territory2), 5)
  val board = Board("game1", Set(continent.copy(territories = Set(territory1, territory2)), Continent("Asia", Set(territory3), 3)))

  def createGameState(tm: TurnManager): GameState = GameState(
    gameId = "test",
    board = board,
    playerStates = List(playerState, otherState),
    turnManager = tm,
    decksManager = DecksManagerImpl(List.empty, List.empty),
    objectiveCards = List.empty
  )

  def createEngineState(
    gs: GameState,
    pendingAttack: Option[(PlayerImpl, PlayerImpl, Territory, Territory, Int)] = None
  ): EngineState =
    EngineState(gs, pendingAttack, false)

  val turnManager = new TurnManager:
    def currentPlayer: Player = player
    def nextPlayer(): TurnManager = this
    def currentPhase: TurnPhase = TurnPhase.WaitingForTurn
    def nextPhase(): TurnManager = this
    def isValidAction(action: GameAction, gs: GameState, es: EngineState): Boolean = true

  val decksManager = DecksManagerImpl(List(), List())

  val gameState = GameState(
    gameId = "game1",
    board = board,
    playerStates = List(playerState, otherState),
    turnManager = turnManager,
    decksManager = decksManager,
    objectiveCards = List.empty
  )

  test("getPlayerState returns the correct player"):
    assert(gameState.getPlayerState("1").exists(_.playerId == "1"))

  test("updatePlayerState updates the player state"):
    val updated = playerState.copy(phase = TurnPhase.Attacking)
    val newGameState = gameState.updatePlayerState("1", updated)
    assert(newGameState.playerStates.exists(_.phase == TurnPhase.Attacking))

  test("updateBoard updates the board"):
    val newBoard = board.copy(gameId = "newGame")
    val newGameState = gameState.updateBoard(newBoard)
    assert(newGameState.board.gameId == "newGame")

  test("updateTurnManager updates the turn manager"):
    val newTurnManager = new TurnManager:
      def currentPlayer: Player = otherPlayer
      def nextPlayer(): TurnManager = this
      def currentPhase: TurnPhase = TurnPhase.Attacking
      def nextPhase(): TurnManager = this
      def isValidAction(action: GameAction, gs: GameState, es: EngineState): Boolean = true
    val newGameState = gameState.updateTurnManager(newTurnManager)
    assert(newGameState.turnManager.currentPlayer == otherPlayer)

  test("checkWinCondition returns winner if objective is completed"):
    val obj = ObjectiveCard.ConquerTerritories(2, 2)
    val winningPlayerState = playerState.copy(objectiveCard = Some(obj))
    val gs = gameState.copy(playerStates = List(winningPlayerState, otherState))
    assert(gs.checkWinCondition.exists(_.playerId == "1"))

  test("checkWinCondition returns None if no one has won"):
    val obj = ObjectiveCard.ConquerTerritories(5, 2)
    val nonWinningPlayerState = playerState.copy(objectiveCard = Some(obj))
    val gs = gameState.copy(playerStates = List(nonWinningPlayerState, otherState))
    assert(gs.checkWinCondition.isEmpty)