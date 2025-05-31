package engine

import model.board.Territory
import model.player.Player
import utils.Dice

object BattleResult extends Enumeration {
  type BattleResult = Value
  val AttackerWins, DefenderWins, Ongoing = Value
}
import BattleResult._

case class BattleState(
  attacker: Player,
  defender: Player,
  attackingTerritory: Territory,
  defendingTerritory: Territory,
  attackingArmies: Int,
  defendingArmies: Int
)

object Battle {

  def resolveBattleRound(state: BattleState): BattleState = {
    val attackerDice = Dice.roll(math.min(3, state.attackingArmies))
    val defenderDice = Dice.roll(math.min(3, state.defendingArmies))

    val pairs = attackerDice.zip(defenderDice)
    val (attackerLosses, defenderLosses) = pairs.foldLeft((0, 0)) {
      case ((aLoss, dLoss), (aDie, dDie)) =>
        if (aDie > dDie) (aLoss, dLoss + 1)
        else (aLoss + 1, dLoss)
    }

    state.copy(
      attackingArmies = state.attackingArmies - attackerLosses,
      defendingArmies = state.defendingArmies - defenderLosses
    )
  }

  def battle(
    attacker: Player,
    defender: Player,
    attackingTerritory: Territory,
    defendingTerritory: Territory,
    attackingArmies: Int
  ): (BattleResult, Territory, Territory) = {
    require(attackingTerritory.owner == attacker, "Attacker must own the attacking territory")
    require(defendingTerritory.owner == defender, "Defender must own the defending territory")
    require(attackingArmies > 0 && attackingArmies < attackingTerritory.armies, "Invalid number of attacking armies")

    var state = BattleState(
      attacker,
      defender,
      attackingTerritory,
      defendingTerritory,
      attackingArmies,
      defendingTerritory.armies
    )

    while (state.attackingArmies > 0 && state.defendingArmies > 0) {
      state = resolveBattleRound(state)
    }

    if (state.defendingArmies == 0) {
      val conquered = defendingTerritory.copy(owner = attacker, armies = state.attackingArmies)
      val updatedAttacking = attackingTerritory.copy(armies = attackingTerritory.armies - attackingArmies)
      (AttackerWins, updatedAttacking, conquered)
    } else {
      val updatedAttacking = attackingTerritory.copy(armies = attackingTerritory.armies - (attackingArmies - state.attackingArmies))
      val updatedDefending = defendingTerritory.copy(armies = state.defendingArmies)
      (DefenderWins, updatedAttacking, updatedDefending)
    }
  }
}