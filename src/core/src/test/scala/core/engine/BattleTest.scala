package engine

import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers
import engine.BattleResult
import engine.BattleResult._
import model.player._
import model.board._
import exceptions._

class BattleTest extends AnyFunSuite with Matchers:
  
  val attacker = PlayerImpl("1", "Attacker", PlayerColor.Red, PlayerType.Human)
  val defender = PlayerImpl("2", "Defender", PlayerColor.Blue, PlayerType.Human)

  val attackerTerritory = Territory(
    name = "A",
    owner = Some(attacker),
    troops = 10,
    neighbors = Set.empty
  )
  val defenderTerritory = Territory(
    name = "B",
    owner = Some(defender),
    troops = 3,
    neighbors = Set.empty
  )
  
  test("Defender should loose troops if he loose the battle"):
    val result = Battle.battleRound(
      attacker,
      defender,
      attackerTerritory,
      defenderTerritory,
      attackingTroops = 3
    )
    if result.result == BattleResult.AttackerWins then
      result.defenderTerritory.owner should contain (attacker)
      result.attackerTerritory.troops shouldBe (attackerTerritory.troops - 3)
      result.defenderTerritory.troops shouldBe 3
  
  test("Defender should keep his territory if he wins or battle continues"):
    val result = Battle.battleRound(
      attacker,
      defender,
      attackerTerritory.copy(troops = 3),
      defenderTerritory.copy(troops = 5),
      attackingTroops = 2
    )
    if result.result == BattleResult.DefenderWins || result.result == BattleResult.BattleContinues then
      result.defenderTerritory.owner should contain (defender)
      result.attackerTerritory.owner should contain (attacker)
  
  test("Cannot attack with all troops"):
    an [InvalidActionException] should be thrownBy {
      Battle.battleRound(
        attacker,
        defender,
        attackerTerritory.copy(troops = 3),
        defenderTerritory.copy(troops = 2),
        attackingTroops = 3 
      )
    }
    
  test("Cannot attack from a not-owned territory"):
    val notOwnedTerritory = Territory("C", Some(defender), 5, Set.empty)
    
    an [InvalidActionException] should be thrownBy {
      Battle.battleRound(
        attacker,
        defender,
        notOwnedTerritory,  
        defenderTerritory, 
        attackingTroops = 3
      )
    }
    
  test("Cannot attack a not-owned territory"):
    val neutralTerritory = Territory("D", None, 3, Set.empty)
    
    an [InvalidActionException] should be thrownBy {
      Battle.battleRound(
        attacker,
        defender,
        attackerTerritory,  
        neutralTerritory,
        attackingTroops = 3
      )
    }
  
  test("Battle round returns correct data structure"):
    val result = Battle.battleRound(
      attacker, 
      defender, 
      attackerTerritory, 
      defenderTerritory, 
      3
    )
    result shouldBe a [BattleRoundResult]
    result.attackerDice.size should be <= 3
    result.defenderDice.size should be <= 3
  
  test("Attacker should move his attacking troops on conquered territory when winning"):
    val attackingTroops = 3
    val result = Battle.battleRound(
      attacker,
      defender,
      attackerTerritory,
      defenderTerritory,
      attackingTroops
    )
    
    if result.result == BattleResult.AttackerWins then
      result.attackerTerritory.troops shouldBe (attackerTerritory.troops - attackingTroops) 
      result.defenderTerritory.troops shouldBe attackingTroops                       
      result.defenderTerritory.owner shouldBe Some(attacker)