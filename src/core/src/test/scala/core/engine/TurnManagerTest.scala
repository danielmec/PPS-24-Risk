package core.engine

import org.scalatest.funsuite.AnyFunSuite
import model.player.*
import model.cards.*
import engine.*

class TurnManagerTest extends AnyFunSuite:

  val player1 = PlayerImpl("1", "Alice", PlayerColor.Red, PlayerType.Human)
  val player2 = PlayerImpl("2", "Bob", PlayerColor.Blue, PlayerType.Human)
  val players = List(player1, player2)

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
    assert(tm.isValidAction(GameAction.PlaceTroops("1", 3)))
    assert(!tm.isValidAction(GameAction.PlaceTroops("2", 3)))
    assert(!tm.isValidAction(GameAction.PlaceTroops("1", 0)))

  test("isValidAction for Reinforce in Reinforcement phase"):
    val tm = TurnManagerImpl(players, phase = TurnPhase.Reinforcement)
    assert(tm.isValidAction(GameAction.Reinforce("1", 2)))
    assert(!tm.isValidAction(GameAction.Reinforce("2", 2)))
    assert(!tm.isValidAction(GameAction.Reinforce("1", 0)))

  test("isValidAction for TradeCards in Reinforcement phase"):
    val tm = TurnManagerImpl(players, phase = TurnPhase.Reinforcement)
    assert(tm.isValidAction(GameAction.TradeCards(Set.empty)))

  test("isValidAction for Attack in Attacking phase"):
    val tm = TurnManagerImpl(players, phase = TurnPhase.Attacking)
    assert(tm.isValidAction(GameAction.Attack("1", "2")))
    assert(!tm.isValidAction(GameAction.Attack("2", "1")))
    assert(!tm.isValidAction(GameAction.Attack("1", "1")))

  test("isValidAction for Defend in Defending phase"):
    val tm = TurnManagerImpl(players, phase = TurnPhase.Defending)
    assert(tm.isValidAction(GameAction.Defend("2", 2)))
    assert(!tm.isValidAction(GameAction.Defend("1", 2)))
    assert(!tm.isValidAction(GameAction.Defend("2", 0)))
    assert(!tm.isValidAction(GameAction.Defend("2", 4)))

  test("isValidAction for EndAttack, EndPhase, EndTurn"):
    val tm = TurnManagerImpl(players, phase = TurnPhase.Attacking)
    assert(tm.isValidAction(GameAction.EndAttack))
    assert(tm.isValidAction(GameAction.EndPhase))
    assert(tm.isValidAction(GameAction.EndTurn))

  test("isValidAction returns false for invalid actions"):
    val tm = TurnManagerImpl(players, phase = TurnPhase.PlacingTroops)
    assert(!tm.isValidAction(GameAction.Attack("1", "2")))
    assert(!tm.isValidAction(GameAction.Defend("2", 2)))
