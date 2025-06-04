package core.engine

import org.scalatest.funsuite.AnyFunSuite
import model.cards.*
import model.player.*
import model.board.*

class GameStateTest extends AnyFunSuite:

  val player = PlayerImpl("1", "Alice", PlayerColor.Red, PlayerType.Human)
  val playerState = PlayerState(player, Set.empty, None)
  val otherPlayer = PlayerImpl("2", "Bob", PlayerColor.Blue, PlayerType.Human)
  val otherState = PlayerState(otherPlayer, Set.empty, None)

  val territory1 = Territory("T1", Set.empty, Some(player), 3)
  val territory2 = Territory("T2", Set.empty, Some(player), 2)
  val territory3 = Territory("T3", Set.empty, Some(otherPlayer), 1)
  val continent = Continent("Europe", Set(territory1, territory2))
  val board = Board("game1", Set(continent.copy(territories = Set(territory1, territory2)), Continent("Asia", Set(territory3))))

  // Dummy managers
  val turnManager = new TurnManager {
    def currentPlayer: Player = player
    def nextPlayer(): TurnManager = this
    def currentPhase: TurnPhase = TurnPhase.WaitingForTurn
    def nextPhase(): TurnManager = this
    def isValidAction(action: GameAction): Boolean = true
  }
  val decksManager = null

  val gameState = GameState(
    gameId = "game1",
    board = board,
    playerStates = List(playerState, otherState),
    turnManager = turnManager,
    decksManager = decksManager
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
    val newTurnManager = new TurnManager
      def currentPlayer: Player = otherPlayer
      def nextPlayer(): TurnManager = this
      def currentPhase: TurnPhase = TurnPhase.Attacking
      def nextPhase(): TurnManager = this
      def isValidAction(action: GameAction): Boolean = true
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