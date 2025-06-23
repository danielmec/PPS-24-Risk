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

  @annotation.tailrec
  private def battleLoop(
    state: BattleState,
    attackerDiceRoll: Int => Seq[Int],
    defenderDiceRoll: Int => Seq[Int]
  ): BattleState =
    if (state.attackingTroops <= 0 || state.defendingTroops <= 0) state
    else
      val newState = resolveBattleRound(state, attackerDiceRoll, defenderDiceRoll)
      battleLoop(newState, attackerDiceRoll, defenderDiceRoll)

  private def validateBattle(
    attacker: Player,
    defender: Player,
    attackerTerritory: Territory,
    defenderTerritory: Territory,
    attackingTroops: Int
  ): Either[String, Unit] =
    if (!attackerTerritory.isOwnedBy(attacker.id))
      Left("Attacker does not own the attacking territory")
    else if (!defenderTerritory.isOwnedBy(defender.id))
      Left("Defender does not own the defending territory")
    else if (attackingTroops <= 0)
      Left("Must attack with at least 1 troop")
    else if (attackingTroops >= attackerTerritory.troops)
      Left("Cannot attack with all troops - must leave at least 1 to defend")
    else
      Right(())

  private def createBattleResult(
    finalState: BattleState,
    originalAttackingTroops: Int
  ): (BattleResult, Territory, Territory) =
    val BattleState(attacker, defender, attackerTerritory, defenderTerritory, _, _) = finalState
    
    if (finalState.defendingTroops == 0) {
      val conqueredTerritory = defenderTerritory
        .changeOwner(attacker)
        .copy(troops = finalState.attackingTroops)
      val updatedAttackerTerritory = attackerTerritory
        .copy(troops = attackerTerritory.troops - originalAttackingTroops)
      (AttackerWins, updatedAttackerTerritory, conqueredTerritory)
    } else {
      val troopsLost = originalAttackingTroops - finalState.attackingTroops
      val updatedAttackerTerritory = attackerTerritory
        .copy(troops = attackerTerritory.troops - troopsLost)
      val updatedDefenderTerritory = defenderTerritory
        .copy(troops = finalState.defendingTroops)
      (DefenderWins, updatedAttackerTerritory, updatedDefenderTerritory)
    }

  def battle(
    attacker: Player,
    defender: Player,
    attackerTerritory: Territory,
    defenderTerritory: Territory,
    attackingTroops: Int,
    attackerDiceRoll: Int => Seq[Int] = Dice.roll,
    defenderDiceRoll: Int => Seq[Int] = Dice.roll
  ): (BattleResult, Territory, Territory) =
    
    validateBattle(attacker, defender, attackerTerritory, defenderTerritory, attackingTroops) match
      case Left(error) => throw new InvalidActionException()
      case Right(_) =>
        val initialState = BattleState(
          attacker,
          defender,
          attackerTerritory,
          defenderTerritory,
          attackingTroops,
          defenderTerritory.troops
        )
        
        val finalState = battleLoop(initialState, attackerDiceRoll, defenderDiceRoll)
        
        createBattleResult(finalState, attackingTroops)

  import scala.util.{Try, Success, Failure}
  
  def battleSafe(
    attacker: Player,
    defender: Player,
    attackerTerritory: Territory,
    defenderTerritory: Territory,
    attackingTroops: Int,
    attackerDiceRoll: Int => Seq[Int] = Dice.roll,
    defenderDiceRoll: Int => Seq[Int] = Dice.roll
  ): Try[(BattleResult, Territory, Territory)] =
    Try {
      validateBattle(attacker, defender, attackerTerritory, defenderTerritory, attackingTroops) match
        case Left(error) => throw new InvalidActionException()
        case Right(_) =>
          val initialState = BattleState(
            attacker, defender, attackerTerritory, defenderTerritory,
            attackingTroops, defenderTerritory.troops
          )
          val finalState = battleLoop(initialState, attackerDiceRoll, defenderDiceRoll)
          createBattleResult(finalState, attackingTroops)
    }

  def battleEither(
    attacker: Player,
    defender: Player,
    attackerTerritory: Territory,
    defenderTerritory: Territory,
    attackingTroops: Int,
    attackerDiceRoll: Int => Seq[Int] = Dice.roll,
    defenderDiceRoll: Int => Seq[Int] = Dice.roll
  ): Either[String, (BattleResult, Territory, Territory)] =
    validateBattle(attacker, defender, attackerTerritory, defenderTerritory, attackingTroops) match
      case Left(error) => Left(error)
      case Right(_) =>
        val initialState = BattleState(
          attacker, defender, attackerTerritory, defenderTerritory,
          attackingTroops, defenderTerritory.troops
        )
        val finalState = battleLoop(initialState, attackerDiceRoll, defenderDiceRoll)
        Right(createBattleResult(finalState, attackingTroops))