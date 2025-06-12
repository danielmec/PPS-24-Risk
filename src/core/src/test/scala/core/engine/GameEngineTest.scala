package engine

import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.BeforeAndAfterEach
import model.player.*
import model.cards.*
import model.board.*
import utils.TestUtils.*

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

  test("PlaceTroops - places troops correctly and reduces bonus"):
    val territoryName = engine.getGameState.board.territories.find(_.owner.exists(_.id == "1")).get.name
    val result = engine.performAction(GameAction.PlaceTroops("1", 3, territoryName))
    assert(result.isRight)
    val gameState = result.getOrElse(fail("Action failed"))
    val updatedTerritory = gameState.board.territories.find(_.name == territoryName).get
    val playerState = gameState.playerStates.find(_.playerId == "1").get
    assert(updatedTerritory.troops == 6)
    assert(playerState.bonusTroops == 2)

  test("PlaceTroops - fails if too many troops"):
    val territoryName = engine.getGameState.board.territories.find(_.owner.exists(_.id == "1")).get.name
    assert(engine.performAction(GameAction.PlaceTroops("1", 10, territoryName)).isLeft)

  test("PlaceTroops - fails if zero or negative troops"):
    val territoryName = engine.getGameState.board.territories.find(_.owner.exists(_.id == "1")).get.name
    assert(engine.performAction(GameAction.PlaceTroops("1", 0, territoryName)).isLeft)
    assert(engine.performAction(GameAction.PlaceTroops("1", -1, territoryName)).isLeft)

  test("PlaceTroops - fails if territory not owned"):
    val emptyTerritoryName = engine.getGameState.board.territories.find(_.owner.isEmpty).get.name
    assert(engine.performAction(GameAction.PlaceTroops("1", 2, emptyTerritoryName)).isLeft)

  test("PlaceTroops - fails if territory does not exist"):
    assert(engine.performAction(GameAction.PlaceTroops("1", 2, "NonExistentTerritory")).isLeft)

  test("PlaceTroops - fails if player does not exist"):
    val territoryName = engine.getGameState.board.territories.find(_.owner.exists(_.id == "1")).get.name
    assert(engine.performAction(GameAction.PlaceTroops("999", 2, territoryName)).isLeft)

  test("Start turn bonus is territories/3 rounded down (min 3)"):
    val allTerritories = engine.getGameState.board.territories.toList
    val updatedBoard = assignTerritoriesToPlayer(engine.getGameState.board, allTerritories.take(9), player1)
    engine.setGameState(engine.getGameState.copy(
      board = updatedBoard,
      playerStates = updatePlayerBonus(engine.getGameState.playerStates, "1", 0),
      turnManager = resetTurnManager(List(player1, player2))
    ))
    val result = engine.performAction(GameAction.EndTurn)
    assert(result.isRight)
    val playerState = result.getOrElse(fail("Action failed")).playerStates.find(_.playerId == "1").get
    assert(playerState.bonusTroops == 3)

  test("Start turn bonus includes continent bonus if fully owned"):
    val maybeEurope = engine.getGameState.board.continents.find(_.name.equalsIgnoreCase("Europe"))
    assume(maybeEurope.isDefined, "Continent Europe must exist in the test board")
    val europe = maybeEurope.get
    val updatedBoard = assignTerritoriesToPlayer(engine.getGameState.board, europe.territories.toList, player1)
    engine.setGameState(engine.getGameState.copy(
      board = updatedBoard,
      playerStates = updatePlayerBonus(engine.getGameState.playerStates, "1", 0),
      turnManager = resetTurnManager(List(player1, player2))
    ))
    val result = engine.performAction(GameAction.EndTurn)
    assert(result.isRight)
    val playerState = result.getOrElse(fail("Action failed")).playerStates.find(_.playerId == "1").get
    val expectedBonus = math.max(3, europe.territories.size / 3) + europe.bonusTroops
    assert(playerState.bonusTroops == expectedBonus)

  test("Reinforce - moves troops correctly between adjacent territories"):
    val allTerritories = engine.getGameState.board.territories.toList
    val existingT1 = allTerritories.head
    val existingT2 = allTerritories.find(t => existingT1.neighbors.exists(_.name == t.name))
      .getOrElse(allTerritories.tail.head)
    
    val t1 = existingT1.copy(
      owner = Some(player1), 
      troops = 4
    )
    val t2 = existingT2.copy(
      owner = Some(player1), 
      troops = 2
    )
    
    val updatedBoard = engine.getGameState.board
      .updatedTerritory(t1)
      .updatedTerritory(t2)

    engine.setGameState(engine.getGameState.copy(
      board = updatedBoard,
      turnManager = resetTurnManager(List(player1, player2), TurnPhase.Reinforcement)
    ))

    val result = engine.performAction(GameAction.Reinforce("1", t1.name, t2.name, 1))
    
    assert(result.isRight)
    val gameState = result.getOrElse(fail("Action failed"))
    val updatedT1Result = gameState.board.territories.find(_.name == t1.name).get
    val updatedT2Result = gameState.board.territories.find(_.name == t2.name).get
    
    assert(updatedT1Result.troops == 3) // 4 - 1
    assert(updatedT2Result.troops == 3) // 2 + 1

  test("Reinforce - fails if territories are not adjacent"):
    val allTerritories = engine.getGameState.board.territories.toList
    val t1 = allTerritories.head.copy(
      name = "Territory1",
      owner = Some(player1), 
      troops = 5,
      neighbors = Set.empty
    )
    val t2 = allTerritories.tail.head.copy(
      name = "Territory2",
      owner = Some(player1), 
      troops = 2,
      neighbors = Set.empty
    )
    
    val updatedBoard = engine.getGameState.board
      .updatedTerritory(t1)
      .updatedTerritory(t2)

    engine.setGameState(engine.getGameState.copy(
      board = updatedBoard,
      turnManager = resetTurnManager(List(player1, player2), TurnPhase.Reinforcement)
    ))

    val result = engine.performAction(GameAction.Reinforce("1", "Territory1", "Territory2", 2))
    
    assert(result.isLeft)

  test("Reinforce - fails if territories not owned by same player"):
    val allTerritories = engine.getGameState.board.territories.toList
    val t1 = allTerritories.head.copy(
      name = "Territory1",
      owner = Some(player1), 
      troops = 5
    )
    val t2 = allTerritories.tail.head.copy(
      name = "Territory2",
      owner = Some(player2),
      troops = 2,
      neighbors = Set(t1)
    )
    val updatedT1 = t1.copy(neighbors = Set(t2))
    
    val updatedBoard = engine.getGameState.board
      .updatedTerritory(updatedT1)
      .updatedTerritory(t2)

    engine.setGameState(engine.getGameState.copy(
      board = updatedBoard,
      turnManager = resetTurnManager(List(player1, player2), TurnPhase.Reinforcement)
    ))

    val result = engine.performAction(GameAction.Reinforce("1", "Territory1", "Territory2", 2))
    
    assert(result.isLeft)

  test("Reinforce - fails if not enough troops to move"):
    val allTerritories = engine.getGameState.board.territories.toList
    val t1 = allTerritories.head.copy(
      name = "Territory1",
      owner = Some(player1), 
      troops = 3
    )
    val t2 = allTerritories.tail.head.copy(
      name = "Territory2",
      owner = Some(player1), 
      troops = 2,
      neighbors = Set(t1)
    )
    val updatedT1 = t1.copy(neighbors = Set(t2))
    
    val updatedBoard = engine.getGameState.board
      .updatedTerritory(updatedT1)
      .updatedTerritory(t2)

    engine.setGameState(engine.getGameState.copy(
      board = updatedBoard,
      turnManager = resetTurnManager(List(player1, player2), TurnPhase.Reinforcement)
    ))

    val result = engine.performAction(GameAction.Reinforce("1", "Territory1", "Territory2", 3))
    
    assert(result.isLeft)

  test("Reinforce - fails if zero or negative troops"):
    val allTerritories = engine.getGameState.board.territories.toList
    val t1 = allTerritories.head.copy(
      name = "Territory1",
      owner = Some(player1), 
      troops = 5
    )
    val t2 = allTerritories.tail.head.copy(
      name = "Territory2",
      owner = Some(player1), 
      troops = 2,
      neighbors = Set(t1)
    )
    val updatedT1 = t1.copy(neighbors = Set(t2))
    
    val updatedBoard = engine.getGameState.board
      .updatedTerritory(updatedT1)
      .updatedTerritory(t2)

    engine.setGameState(engine.getGameState.copy(
      board = updatedBoard,
      turnManager = resetTurnManager(List(player1, player2), TurnPhase.Reinforcement)
    ))

    assert(engine.performAction(GameAction.Reinforce("1", "Territory1", "Territory2", 0)).isLeft)
    assert(engine.performAction(GameAction.Reinforce("1", "Territory1", "Territory2", -1)).isLeft)

  test("Reinforce - fails if territories do not exist"):
    engine.setGameState(engine.getGameState.copy(
      turnManager = resetTurnManager(List(player1, player2), TurnPhase.Reinforcement)
    ))

    val result = engine.performAction(GameAction.Reinforce("1", "NonExistent1", "NonExistent2", 1))
    
    assert(result.isLeft)

