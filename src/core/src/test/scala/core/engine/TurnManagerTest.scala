package core.engine

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
    EngineState(gs, None, false)

  test("currentPlayer returns the correct player"):
    val tm = TurnManagerImpl(players)
    assert(tm.currentPlayer == player1)
    assert(tm.nextPlayer().currentPlayer == player2)

  test("nextPlayer wraps around to first player"):
    val tm = TurnManagerImpl(players, currentPlayerIndex = 1)
    val next = tm.nextPlayer()
    assert(next.currentPlayer == player1)

  test("currentPhase and nextPhase cycle correctly"):
    val tm = TurnManagerImpl(players)
    assert(tm.currentPhase == TurnPhase.WaitingForTurn)
    val tm2 = tm.nextPhase()
    assert(tm2.currentPhase == TurnPhase.PlacingTroops)
    val tm3 = tm2.nextPhase()
    assert(tm3.currentPhase == TurnPhase.Reinforcement)
    val tm4 = tm3.nextPhase()
    assert(tm4.currentPhase == TurnPhase.Attacking)
    val tm5 = tm4.nextPhase()
    assert(tm5.currentPhase == TurnPhase.Defending)
    val tm6 = tm5.nextPhase()
    assert(tm6.currentPhase == TurnPhase.WaitingForTurn)

  test("isValidAction for PlaceTroops in PlacingTroops phase"):
    val tm = TurnManagerImpl(players, phase = TurnPhase.PlacingTroops)
    val gs = createGameState(tm)
    val es = createEngineState(gs)
    assert(tm.isValidAction(GameAction.PlaceTroops("1", 3, "TerritoryA"), gs, es))
    assert(!tm.isValidAction(GameAction.PlaceTroops("2", 3, "TerritoryA"), gs, es))
    assert(!tm.isValidAction(GameAction.PlaceTroops("1", 0, "TerritoryA"), gs, es))

  test("isValidAction for Reinforce in Reinforcement phase"):
    val tm = TurnManagerImpl(players, phase = TurnPhase.Reinforcement)
    val gs = createGameState(tm)
    val es = createEngineState(gs)
    assert(tm.isValidAction(GameAction.Reinforce("1", "TerritoryA", "TerritoryB", 2), gs, es))
    assert(!tm.isValidAction(GameAction.Reinforce("2", "TerritoryA", "TerritoryB", 2), gs, es))
    assert(!tm.isValidAction(GameAction.Reinforce("1", "TerritoryA", "TerritoryB", 0), gs, es))

  test("isValidAction for TradeCards in Reinforcement phase"):
    val tm = TurnManagerImpl(players, phase = TurnPhase.Reinforcement)
    val gs = createGameState(tm)
    val es = createEngineState(gs)
    val card1 = TerritoryCard(tA, CardImg.Artillery)
    val card2 = TerritoryCard(tB, CardImg.Cavalry)
    val card3 = TerritoryCard(tC, CardImg.Infantry)
    val cards = Set(card1, card2, card3)
    val updatedPlayerState = playerState1.copy(territoryCards = cards)
    val updatedGs = gs.copy(playerStates = List(updatedPlayerState, playerState2))
    assert(tm.isValidAction(GameAction.TradeCards(cards), updatedGs, es))

  test("isValidAction for Attack in Attacking phase"):
    // Per il test di Attack, TerritoryB deve essere di player2
    val attackTerritoryB = tB.copy(owner = Some(player2))
    val attackTerritories = Set(tA, attackTerritoryB, tC)
    val attackContinent = Continent("TestContinent", attackTerritories, bonusTroops = 0)
    val attackBoard = Board("test", Set(attackContinent))
    val tm = TurnManagerImpl(players, phase = TurnPhase.Attacking)
    val playerState1Attack = playerState1
    val playerState2Attack = playerState2
    val gs = GameState(
      gameId = "test",
      board = attackBoard,
      playerStates = List(playerState1Attack, playerState2Attack),
      turnManager = tm,
      decksManager = DecksManagerImpl(List.empty, List.empty),
      objectiveCards = List.empty
    )
    val es = createEngineState(gs)
    assert(tm.isValidAction(GameAction.Attack("1", "2", "TerritoryA", "TerritoryB", 3), gs, es))
    assert(!tm.isValidAction(GameAction.Attack("2", "1", "TerritoryB", "TerritoryA", 3), gs, es))
    assert(!tm.isValidAction(GameAction.Attack("1", "1", "TerritoryA", "TerritoryA", 3), gs, es))

  test("isValidAction for Defend in Defending phase"):
    val tm = TurnManagerImpl(players, phase = TurnPhase.Defending)
    val gs = createGameState(tm)
    val pendingAttack = Some((player1, player2, tA, tC, 3))
    val es = EngineState(gs, pendingAttack, false)
    assert(tm.isValidAction(GameAction.Defend("2", "TerritoryC", 2), gs, es))
    assert(!tm.isValidAction(GameAction.Defend("1", "TerritoryC", 2), gs, es))
    assert(!tm.isValidAction(GameAction.Defend("2", "TerritoryC", 0), gs, es))
    assert(!tm.isValidAction(GameAction.Defend("2", "TerritoryC", 4), gs, es))

  test("isValidAction for EndAttack, EndPhase, EndTurn"):
    val tm = TurnManagerImpl(players, phase = TurnPhase.Attacking)
    val gs = createGameState(tm)
    val es = createEngineState(gs)
    assert(tm.isValidAction(GameAction.EndAttack, gs, es))
    assert(tm.isValidAction(GameAction.EndPhase, gs, es))
    assert(tm.isValidAction(GameAction.EndTurn, gs, es))

  test("isValidAction returns false for invalid actions"):
    val tm = TurnManagerImpl(players, phase = TurnPhase.PlacingTroops)
    val gs = createGameState(tm)
    val es = createEngineState(gs)
    assert(!tm.isValidAction(GameAction.Attack("1", "2", "TerritoryA", "TerritoryB", 3), gs, es))
    assert(!tm.isValidAction(GameAction.Defend("2", "TerritoryA", 2), gs, es))