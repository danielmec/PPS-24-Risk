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

  val attackerWinsDice: Int => Seq[Int] = n => Seq.fill(n)(6)
  val attackerLosesDice: Int => Seq[Int] = n => Seq.fill(n)(1)
  val defenderWinsDice: Int => Seq[Int] = n => Seq.fill(n)(6)
  val defenderLosesDice: Int => Seq[Int] = n => Seq.fill(n)(1)

  val customAttackerDice: Int => Seq[Int] = 
    case 3 => Seq(6, 4, 2)
    case _ => Seq(6)
  
  val customDefenderDice: Int => Seq[Int] = 
    case 2 => Seq(5, 3)
    case _ => Seq(1)
  
  test("Defender should loose troops if he loose the battle"):
    val result = Battle.battle(
      attacker,
      defender,
      attackerTerritory,
      defenderTerritory,
      attackingTroops = 3,
      attackerDiceRoll = attackerWinsDice,
      defenderDiceRoll = defenderLosesDice
    )
    result._1 shouldBe BattleResult.AttackerWins
    result._3.owner should contain (attacker)
    result._2.troops shouldBe (attackerTerritory.troops - 3)
    result._3.troops shouldBe 3
  
  test("Defender should keep his territory if he wins"):
    val result = Battle.battle(
      attacker,
      defender,
      attackerTerritory.copy(troops = 3),
      defenderTerritory.copy(troops = 5),
      attackingTroops = 2,
      attackerDiceRoll = attackerLosesDice,
      defenderDiceRoll = defenderWinsDice
    )
    result._1 shouldBe BattleResult.DefenderWins
    result._3.owner should contain (defender)
    result._2.owner should contain (attacker)
  
  test("Cannot attack with all troops"):
    an [InvalidActionException] should be thrownBy {
      Battle.battle(
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
      Battle.battle(
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
      Battle.battle(
        attacker,
        defender,
        attackerTerritory,  
        neutralTerritory,
        attackingTroops = 3
      )
    }
  
  test("Battle should end correctly"):
    val state = BattleState(attacker, defender, attackerTerritory, defenderTerritory, 3, 2)
    val newState = Battle.resolveBattleRound(state, customAttackerDice, customDefenderDice)
    newState.attackingTroops shouldBe 3  
    newState.defendingTroops shouldBe 0  
  
  test("Attacker should move his attacking troops on conquered territory"):
    val attackingTroops = 3
    val result = Battle.battle(
      attacker,
      defender,
      attackerTerritory,
      defenderTerritory,
      attackingTroops,
      attackerDiceRoll = attackerWinsDice,
      defenderDiceRoll = defenderLosesDice
    )
    result._1 shouldBe BattleResult.AttackerWins
    result._2.troops shouldBe (attackerTerritory.troops - attackingTroops) 
    result._3.troops shouldBe attackingTroops                       
    result._3.owner shouldBe Some(attacker)