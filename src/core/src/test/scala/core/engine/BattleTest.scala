package core.engine

import org.scalatest.funsuite.AnyFunSuite
import engine.*
import model.player.*
import model.board.*

class BattleTest extends AnyFunSuite:

  val attacker = PlayerImpl("1", "Attaccante", PlayerColor.Red, PlayerType.Human)
  val defender = PlayerImpl("2", "Difensore", PlayerColor.Blue, PlayerType.Human)

  val territoryA = Territory(
    name = "A",
    neighbors = Set.empty,
    owner = Some(attacker),
    troops = 10
  )
  val territoryB = Territory(
    name = "B",
    neighbors = Set.empty,
    owner = Some(defender),
    troops = 3
  )

  test("Attaccante conquista il territorio se il difensore perde tutte le truppe") {
    val alwaysSix: Int => Seq[Int] = n => Seq.fill(n)(6)
    val result = Battle.battle(
      attacker,
      defender,
      territoryA,
      territoryB,
      attackingTroops = 3,
      diceRoll = alwaysSix
    )
    assert(result._1 == BattleResult.AttackerWins)
    assert(result._3.owner.contains(attacker))
  }

  test("Difensore mantiene il territorio se l'attaccante si esaurisce") {
    val alwaysOne: Int => Seq[Int] = n => Seq.fill(n)(1)
    val result = Battle.battle(
      attacker,
      defender,
      territoryA.copy(troops = 3),
      territoryB.copy(troops = 5),
      attackingTroops = 2,
      diceRoll = alwaysOne
    )
    assert(result._1 == BattleResult.DefenderWins)
    assert(result._3.owner.contains(defender))
  }

  test("Non si può attaccare con tutte le truppe") {
    intercept[IllegalArgumentException] {
      Battle.battle(
        attacker,
        defender,
        territoryA.copy(troops = 3),
        territoryB.copy(troops = 2),
        attackingTroops = 3 // Non valido: deve restare almeno 1 truppa
      )
    }
  }





