package engine

import org.scalatest.funsuite.AnyFunSuite
import model.player.*
import model.cards.*
import model.board.*
import engine.*

class TurnManagerTest extends AnyFunSuite:
  val player1 = PlayerImpl("1", "Alice", PlayerColor.Red, PlayerType.Human)
  val player2 = PlayerImpl("2", "Bob", PlayerColor.Blue, PlayerType.Human)
  val players = List(player1, player2)
  
  val territoryA = Territory("TerritoryA", Some(player1), 5)
  val territoryB = Territory("TerritoryB", Some(player1), 5)
  val territoryC = Territory("TerritoryC", Some(player2), 3)
  val tA = territoryA.copy(neighbors = Set(territoryB))
  val tB = territoryB.copy(neighbors = Set(tA))
  val tC = territoryC.copy(neighbors = Set())
  val territories = Set(tA, tB, tC)
  val continent = Continent("TestContinent", territories, bonusTroops = 0)
  val board = Board("test", Set(continent))
  
  val playerState1 = PlayerState(player1, bonusTroops = 10)
  val playerState2 = PlayerState(player2, bonusTroops = 5)

  def createGameState(tm: TurnManager): GameState = GameState(
    gameId = "test",
    board = board,
    playerStates = List(playerState1, playerState2),
    turnManager = tm,
    decksManager = DecksManagerImpl(List.empty, List.empty),
    objectiveCards = List.empty
  )

  def createEngineState(gs: GameState): EngineState =
    EngineState(gs, false)
    
  def createTurnManager(phase: TurnPhase, playerIdx: Int = 0): TurnManagerImpl =
    TurnManagerImpl(players, currentPlayerIndex = playerIdx, phase = phase)
    
  def createAttackBoard(): (Board, Territory, Territory) =
    val attackTerritoryB = tB.copy(owner = Some(player2))
    val attackTerritories = Set(tA, attackTerritoryB, tC)
    val attackContinent = Continent("TestContinent", attackTerritories, bonusTroops = 0)
    val attackBoard = Board("test", Set(attackContinent))
    (attackBoard, tA, attackTerritoryB)

  test("currentPlayer returns the correct player"):
    val tm = createTurnManager(TurnPhase.SetupPhase)
    assert(tm.currentPlayer == player1)
    assert(tm.nextPlayer().currentPlayer == player2)

  test("nextPlayer wraps around to first player"):
    val tm = createTurnManager(TurnPhase.SetupPhase, playerIdx = 1)
    val next = tm.nextPlayer()
    assert(next.currentPlayer == player1)

  test("currentPhase starts with SetupPhase and changes to MainPhase after a full round"):
    val tm = createTurnManager(TurnPhase.SetupPhase)
    assert(tm.currentPhase == TurnPhase.SetupPhase)
    val tm2 = tm.nextPlayer() // Move to player 2
    assert(tm2.currentPhase == TurnPhase.SetupPhase) 
    val tm3 = tm2.nextPlayer() // Back to player 1, should change to MainPhase
    assert(tm3.currentPhase == TurnPhase.MainPhase)

  // Action validation tests
  test("isValidAction for PlaceTroops in SetupPhase"):
    val tm = createTurnManager(TurnPhase.SetupPhase)
    val gs = createGameState(tm)
    val es = createEngineState(gs)
    
    assert(tm.isValidAction(GameAction.PlaceTroops("1", 3, "TerritoryA"), gs, es))
    assert(!tm.isValidAction(GameAction.PlaceTroops("2", 3, "TerritoryA"), gs, es))
    assert(!tm.isValidAction(GameAction.PlaceTroops("1", 0, "TerritoryA"), gs, es))

  test("isValidAction for PlaceTroops in MainPhase"):
    val tm = createTurnManager(TurnPhase.MainPhase)
    val gs = createGameState(tm)
    val es = createEngineState(gs)
    
    assert(tm.isValidAction(GameAction.PlaceTroops("1", 3, "TerritoryA"), gs, es))  
    assert(!tm.isValidAction(GameAction.PlaceTroops("1", 11, "TerritoryA"), gs, es)) // More than available bonus
    assert(!tm.isValidAction(GameAction.PlaceTroops("2", 3, "TerritoryA"), gs, es)) // Not current player

  test("isValidAction for Reinforce in MainPhase"):
    val tm = createTurnManager(TurnPhase.MainPhase)
    val playerState1NoBonusTroops = playerState1.copy(bonusTroops = 0)
    val gs = GameState(
      gameId = "test",
      board = board,
      playerStates = List(playerState1NoBonusTroops, playerState2),
      turnManager = tm,
      decksManager = DecksManagerImpl(List.empty, List.empty),
      objectiveCards = List.empty
    )
    val es = createEngineState(gs)
    
    assert(tm.isValidAction(GameAction.Reinforce("1", "TerritoryA", "TerritoryB", 2), gs, es))
    assert(tm.isValidAction(GameAction.Reinforce("1", "TerritoryA", "TerritoryB", 0), gs, es))
    
    assert(!tm.isValidAction(GameAction.Reinforce("2", "TerritoryA", "TerritoryB", 2), gs, es))

  test("isValidAction for TradeCards in MainPhase"):
    val tm = createTurnManager(TurnPhase.MainPhase)
    
    val cardSet = Set(
      TerritoryCard(tA, CardImg.Artillery),
      TerritoryCard(tB, CardImg.Cavalry),
      TerritoryCard(tC, CardImg.Infantry)
    )
    
    val updatedPlayerState = playerState1.copy(territoryCards = cardSet)
    val gs = GameState(
      gameId = "test",
      board = board,
      playerStates = List(updatedPlayerState, playerState2),
      turnManager = tm,
      decksManager = DecksManagerImpl(List.empty, List.empty),
      objectiveCards = List.empty
    )
    val es = createEngineState(gs)
    
    assert(tm.isValidAction(GameAction.TradeCards(cardSet), gs, es))

  test("isValidAction for Attack in MainPhase"):
    val tm = createTurnManager(TurnPhase.MainPhase)
    val (attackBoard, attackerTerritory, defenderTerritory) = createAttackBoard()
    
    val playerState1NoBonusTroops = playerState1.copy(bonusTroops = 0)
    val gs = GameState(
      gameId = "test",
      board = attackBoard,
      playerStates = List(playerState1NoBonusTroops, playerState2),
      turnManager = tm,
      decksManager = DecksManagerImpl(List.empty, List.empty),
      objectiveCards = List.empty
    )
    val es = createEngineState(gs)
    
    assert(tm.isValidAction(
      GameAction.Attack("1", "2", attackerTerritory.name, defenderTerritory.name, 3), 
      gs, es
    ))
    
    assert(!tm.isValidAction(
      GameAction.Attack("2", "1", defenderTerritory.name, attackerTerritory.name, 3), 
      gs, es
    ))
    assert(!tm.isValidAction(
      GameAction.Attack("1", "1", attackerTerritory.name, attackerTerritory.name, 3), 
      gs, es
    ))

  test("isValidAction for EndTurn"):
    val tm = createTurnManager(TurnPhase.MainPhase)
    val playerState1NoBonusTroops = playerState1.copy(bonusTroops = 0)
    val gs = GameState(
      gameId = "test",
      board = board,
      playerStates = List(playerState1NoBonusTroops, playerState2),
      turnManager = tm,
      decksManager = DecksManagerImpl(List.empty, List.empty),
      objectiveCards = List.empty
    )
    val es = createEngineState(gs)
    
    assert(tm.isValidAction(GameAction.EndTurn, gs, es))
    
  test("isValidAction for EndSetup"):
    val tm = createTurnManager(TurnPhase.SetupPhase)
    val playerState1NoBonusTroops = playerState1.copy(bonusTroops = 0)
    val gs = GameState(
      gameId = "test",
      board = board,
      playerStates = List(playerState1NoBonusTroops, playerState2),
      turnManager = tm,
      decksManager = DecksManagerImpl(List.empty, List.empty),
      objectiveCards = List.empty
    )
    val es = createEngineState(gs)
    
    assert(tm.isValidAction(GameAction.EndSetup, gs, es))

  test("isValidAction returns false for invalid actions in current phase"):
    val tm = createTurnManager(TurnPhase.SetupPhase)
    val gs = createGameState(tm)
    val es = createEngineState(gs)
    
    assert(!tm.isValidAction(GameAction.Attack("1", "2", "TerritoryA", "TerritoryB", 3), gs, es))