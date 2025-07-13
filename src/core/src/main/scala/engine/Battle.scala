package engine

import model.board.Territory
import model.player.Player
import utils.Dice

/**
  * Enumeration representing the possible results of a battle round.
  */
enum BattleResult:
  /** The attacker wins the battle. */
  case AttackerWins
  /** The defender wins the battle. */
  case DefenderWins
  /** The battle continues for another round. */
  case BattleContinues

import BattleResult._

/**
  * Represents the state of a battle between two players.
  *
  * @param attacker The attacking player.
  * @param defender The defending player.
  * @param attackerTerritory The attacking territory.
  * @param defenderTerritory The defending territory.
  * @param attackingTroops The number of attacking troops.
  * @param defendingTroops The number of defending troops.
  */
case class BattleState(
  attacker: Player,
  defender: Player,
  attackerTerritory: Territory,
  defenderTerritory: Territory,
  attackingTroops: Int,
  defendingTroops: Int
)

/**
  * Represents the result of a single battle round.
  *
  * @param attackerTerritory The updated attacking territory.
  * @param defenderTerritory The updated defending territory.
  * @param attackerLosses The number of troops lost by the attacker.
  * @param defenderLosses The number of troops lost by the defender.
  * @param result The result of the battle round.
  * @param attackerDice The dice rolled by the attacker.
  * @param defenderDice The dice rolled by the defender.
  */
case class BattleRoundResult(
  attackerTerritory: Territory,
  defenderTerritory: Territory,
  attackerLosses: Int,
  defenderLosses: Int,
  result: BattleResult,
  attackerDice: Seq[Int],
  defenderDice: Seq[Int]  
)

/**
  * Object containing utility methods for resolving battles between players.
  */
object Battle:

  /**
    * Resolves a single battle round using the provided dice rolls.
    * @param state The current battle state.
    * @param attackerDice The dice rolled by the attacker.
    * @param defenderDice The dice rolled by the defender.
    * @return The updated battle state after the round.
    */
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

  /**
    * Validates the parameters for a battle.
    * @param attacker The attacking player.
    * @param defender The defending player.
    * @param attackerTerritory The attacking territory.
    * @param defenderTerritory The defending territory.
    * @param attackingTroops The number of attacking troops.
    * @return Either an error message or Unit if valid.
    */
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

  /**
    * Executes a single round of battle between two players.
    * @param attacker The attacking player.
    * @param defender The defending player.
    * @param attackerTerritory The attacking territory.
    * @param defenderTerritory The defending territory.
    * @param attackingTroops The number of attacking troops.
    * @param rollDice The function to roll dice.
    * @return Either an error message or the result of the battle round.
    */
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