package engine

import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.BeforeAndAfterEach
import model.player.*
import model.cards.*
import model.board.*

class GameEngineTest extends AnyFunSuite with BeforeAndAfterEach:

  val player1 = PlayerImpl("1", "Alice", PlayerColor.Red, PlayerType.Human)
  val player2 = PlayerImpl("2", "Bob", PlayerColor.Blue, PlayerType.Human)
  var engine: GameEngine = _

  override def beforeEach(): Unit =
    engine = new GameEngine(List(player1, player2))
    setupTestGameState()

  def setupTestGameState(): Unit =
    val territories = engine.getGameState.board.territories.toList
    val t1 = territories.head.copy(
      owner = Some(player1), 
      troops = 3
    )
    
    val updatedBoard = engine.getGameState.board.updatedTerritory(t1)

    val updatedPlayerStates = engine.getGameState.playerStates.map:
      case ps if ps.playerId == "1" => ps.copy(bonusTroops = 5)
      case ps => ps

    val updatedTurnManager = TurnManagerImpl(
      players = List(player1, player2),
      currentPlayerIndex = 0,
      phase = TurnPhase.PlacingTroops
    )

    engine.setGameState(engine.getGameState.copy(
      board = updatedBoard,
      playerStates = updatedPlayerStates,
      turnManager = updatedTurnManager
    ))

  test("PlaceTroops - piazza truppe correttamente e riduce bonus"):
    val territoryName = engine.getGameState.board.territories.find(_.owner.exists(_.id == "1")).get.name
    val result = engine.performAction(GameAction.PlaceTroops("1", 3, territoryName))
    assert(result.isRight)
    val gameState = result.getOrElse(fail("Azione fallita"))
    val updatedTerritory = gameState.board.territories.find(_.name == territoryName).get
    val playerState = gameState.playerStates.find(_.playerId == "1").get
    assert(updatedTerritory.troops == 6)
    assert(playerState.bonusTroops == 2)

  test("PlaceTroops - fallisce se truppe eccessive"):
    val territoryName = engine.getGameState.board.territories.find(_.owner.exists(_.id == "1")).get.name
    assert(engine.performAction(GameAction.PlaceTroops("1", 10, territoryName)).isLeft)

  test("PlaceTroops - fallisce se truppe zero o negative"):
    val territoryName = engine.getGameState.board.territories.find(_.owner.exists(_.id == "1")).get.name
    assert(engine.performAction(GameAction.PlaceTroops("1", 0, territoryName)).isLeft)
    assert(engine.performAction(GameAction.PlaceTroops("1", -1, territoryName)).isLeft)

  test("PlaceTroops - fallisce se territorio non posseduto"):
    val emptyTerritoryName = engine.getGameState.board.territories.find(_.owner.isEmpty).get.name
    assert(engine.performAction(GameAction.PlaceTroops("1", 2, emptyTerritoryName)).isLeft)

  test("PlaceTroops - fallisce se territorio inesistente"):
    assert(engine.performAction(GameAction.PlaceTroops("1", 2, "TerritorioInesistente")).isLeft)

  test("PlaceTroops - fallisce se giocatore inesistente"):
    val territoryName = engine.getGameState.board.territories.find(_.owner.exists(_.id == "1")).get.name
    assert(engine.performAction(GameAction.PlaceTroops("999", 2, territoryName)).isLeft)
  