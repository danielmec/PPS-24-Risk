package engine

import model.board.Territory
import model.player.Player
import utils.Dice

enum BattleResult:
  case AttackerWins, DefenderWins, BattleContinues

import BattleResult._

case class BattleState(
  attacker: Player,
  defender: Player,
  attackerTerritory: Territory,
  defenderTerritory: Territory,
  attackingTroops: Int,
  defendingTroops: Int
)

case class BattleRoundResult(
  attackerTerritory: Territory,
  defenderTerritory: Territory,
  attackerLosses: Int,
  defenderLosses: Int,
  result: BattleResult,
  attackerDice: Seq[Int],
  defenderDice: Seq[Int]  
)

object Battle:

  def resolveBattleRoundWithDice(state: BattleState, attackerDice: Seq[Int], defenderDice: Seq[Int]): BattleState =
    val pairs = attackerDice.zip(defenderDice)
    val (attackerLosses, defenderLosses) = pairs.foldLeft((0, 0)):
      case ((aLoss, dLoss), (a, d)) =>
        if (a > d) (aLoss, dLoss + 1)
        else (aLoss + 1, dLoss)
    state.copy(
      attackingTroops = state.attackingTroops - attackerLosses,
      defendingTroops = state.defendingTroops - defenderLosses
    )

  def validateBattle(
    attacker: Player,
    defender: Player,
    attackerTerritory: Territory,
    defenderTerritory: Territory,
    attackingTroops: Int
  ): Either[String, Unit] =
    if (!attackerTerritory.isOwnedBy(attacker.id)) Left("Attacker does not own the attacking territory")
    else if (!defenderTerritory.isOwnedBy(defender.id)) Left("Defender does not own the defending territory")
    else if (attackingTroops <= 0) Left("Must attack with at least 1 troop")
    else if (attackingTroops >= attackerTerritory.troops) Left("Cannot attack with all troops - must leave at least 1 to defend")
    else Right(())

  def battleRound(
    attacker: Player,
    defender: Player,
    attackerTerritory: Territory,
    defenderTerritory: Territory,
    attackingTroops: Int
  )(using rollDice: Int => Seq[Int]): Either[String, BattleRoundResult] =

    for
      _ <- validateBattle(attacker, defender, attackerTerritory, defenderTerritory, attackingTroops)
      initialState = BattleState(attacker, defender, attackerTerritory, defenderTerritory, attackingTroops, defenderTerritory.troops)
      attackerDice = rollDice(math.min(3, initialState.attackingTroops)).sorted(Ordering.Int.reverse)
      defenderDice = rollDice(math.min(3, initialState.defendingTroops)).sorted(Ordering.Int.reverse)
      afterRound = resolveBattleRoundWithDice(initialState, attackerDice, defenderDice)
      attackerLosses = attackingTroops - afterRound.attackingTroops
      defenderLosses = defenderTerritory.troops - afterRound.defendingTroops
      result =
        if afterRound.defendingTroops == 0 then AttackerWins
        else if afterRound.attackingTroops == 0 then DefenderWins
        else BattleContinues
      (updatedAttacker, updatedDefender) = result match
        case AttackerWins =>
          val conquered = defenderTerritory.changeOwner(attacker).copy(troops = afterRound.attackingTroops)
          val updatedAttacker = attackerTerritory.copy(troops = attackerTerritory.troops - attackingTroops)
          (updatedAttacker, conquered)
        case DefenderWins =>
          val updatedAttacker = attackerTerritory.copy(troops = attackerTerritory.troops - attackingTroops)
          (updatedAttacker, defenderTerritory)
        case BattleContinues =>
          val updatedAttacker = attackerTerritory.addTroops(-attackerLosses)
          val updatedDefender = defenderTerritory.addTroops(-defenderLosses)
          (updatedAttacker, updatedDefender)
    yield BattleRoundResult(
      updatedAttacker,
      updatedDefender,
      attackerLosses,
      defenderLosses,
      result,
      attackerDice,
      defenderDice
    )

given defaultRollDice: (Int => Seq[Int]) = Dice.rollMany