package engine

import model.board.Territory
import model.player.Player
import utils.Dice
import exceptions._

object BattleResult extends Enumeration:
  type BattleResult = Value
  val AttackerWins, DefenderWins = Value

import BattleResult._

case class BattleState(
  attacker: Player,
  defender: Player,
  attackerTerritory: Territory,
  defenderTerritory: Territory,
  attackingTroops: Int,
  defendingTroops: Int,
)

object Battle:

  def resolveBattleRound(
    state: BattleState,
    attackerDiceRoll: Int => Seq[Int] = Dice.roll,
    defenderDiceRoll: Int => Seq[Int] = Dice.roll
  ): BattleState =
    val attackerDice = attackerDiceRoll(math.min(3, state.attackingTroops))
    val defenderDice = defenderDiceRoll(math.min(3, state.defendingTroops))

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
    attackingTroops: Int,
    attackerDiceRoll: Int => Seq[Int] = Dice.roll,
    defenderDiceRoll: Int => Seq[Int] = Dice.roll
  ): (BattleResult, Territory, Territory) =
    
    if (!attackerTerritory.owner.contains(attacker))
      throw new InvalidActionException()
    if (!defenderTerritory.owner.contains(defender))
      throw new InvalidActionException()
    if (attackingTroops <= 0 || attackingTroops >= attackerTerritory.troops)
      throw new InvalidActionException()

    var state = BattleState(
      attacker,
      defender,
      attackerTerritory,
      defenderTerritory,
      attackingTroops,
      defenderTerritory.troops
    )

    while (state.attackingTroops > 0 && state.defendingTroops > 0)
      state = resolveBattleRound(state, attackerDiceRoll, defenderDiceRoll)

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