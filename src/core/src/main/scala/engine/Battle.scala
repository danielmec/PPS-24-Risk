package engine

import model.board.Territory
import model.player.Player
import utils.Dice

object BattleResult extends Enumeration:
  type BattleResult = Value
  val AttackerWins, DefenderWins, Ongoing = Value

import BattleResult._

case class BattleState(
  attacker: Player,
  defender: Player,
  attackerTerritory: Territory,
  defenderTerritory: Territory,
  attackingTroops: Int,
  defendingTroops: Int
)

object Battle:

  def resolveBattleRound(state: BattleState): BattleState =
    val attackerDice = Dice.roll(math.min(3, state.attackingTroops))
    val defenderDice = Dice.roll(math.min(3, state.defendingTroops))

    val pairs = attackerDice.zip(defenderDice)
    val (attackerLosses, defenderLosses) = pairs.foldLeft((0, 0)): 
      case ((aLoss, dLoss), (aDice, dDice)) =>
        if (aDice > dDice) (aLoss, dLoss + 1)
        else (aLoss + 1, dLoss)

    state.copy(
      attackingTroops = state.attackingTroops - attackerLosses,
      defendingTroops = state.defendingTroops - defenderLosses
    )
  

  def battle(
    attacker: Player,
    defender: Player,
    attackerTerritory: Territory,
    defenderTerritory: Territory,
    attackingTroops: Int
  ): (BattleResult, Territory, Territory) = 
    require(attackerTerritory.owner.contains(attacker), "Attacker must own the attacking territory")
    require(defenderTerritory.owner.contains(defender), "Defender must own the defending territory")
    require(attackingTroops > 0 && attackingTroops < attackerTerritory.troops, "Invalid number of attacking troops")

    var state = BattleState(
      attacker,
      defender,
      attackerTerritory,
      defenderTerritory,
      attackingTroops,
      defenderTerritory.troops
    )

    while (state.attackingTroops > 0 && state.defendingTroops > 0 && state.attackingTroops + 1 <= attackerTerritory.troops)
      state = resolveBattleRound(state)

    if (state.defendingTroops == 0) 
      val conqueredTerritory = defenderTerritory
        .changeOwner(attacker)
        .copy(troops = state.attackingTroops)
      val attackerTerritoryToUpdate = attackerTerritory
        .copy(troops = attackerTerritory.troops - attackingTroops)
      (AttackerWins, attackerTerritoryToUpdate, conqueredTerritory)
    else 
      val attackerTerritoryToUpdate = attackerTerritory
        .copy(troops = attackerTerritory.troops - (attackingTroops - state.attackingTroops))
      val defenderTerritoryToUpdate = defenderTerritory
        .copy(troops = state.defendingTroops)
      (DefenderWins, attackerTerritoryToUpdate, defenderTerritoryToUpdate)


