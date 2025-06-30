package engine

import model.board.Territory
import model.player.Player
import utils.Dice
import exceptions._

object BattleResult extends Enumeration:
  type BattleResult = Value
  val AttackerWins, DefenderWins, BattleContinues = Value // Aggiunto BattleContinues

import BattleResult._

case class BattleState(
  attacker: Player,
  defender: Player,
  attackerTerritory: Territory,
  defenderTerritory: Territory,
  attackingTroops: Int,
  defendingTroops: Int,
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
      case ((aLoss, dLoss), (aDice, dDice)) =>
        if (aDice > dDice) (aLoss, dLoss + 1)
        else (aLoss + 1, dLoss)

    state.copy(
      attackingTroops = state.attackingTroops - attackerLosses,
      defendingTroops = state.defendingTroops - defenderLosses
    )
    
  def resolveBattleRound(state: BattleState): BattleState =
    val attackerDiceCount = math.min(3, state.attackingTroops)
    val defenderDiceCount = math.min(3, state.defendingTroops)
    
    val attackerDice = Dice.rollMany(attackerDiceCount)
    val defenderDice = Dice.rollMany(defenderDiceCount)

    resolveBattleRoundWithDice(state, attackerDice, defenderDice)

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


  def battleRound(
    attacker: Player,
    defender: Player,
    attackerTerritory: Territory,
    defenderTerritory: Territory,
    attackingTroops: Int
  ): BattleRoundResult =
    
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
        
        val attackerDiceCount = math.min(3, initialState.attackingTroops)
        val defenderDiceCount = math.min(3, initialState.defendingTroops)
        
        val attackerDice = Dice.rollMany(attackerDiceCount)
        val defenderDice = Dice.rollMany(defenderDiceCount)
        
        val afterRound = resolveBattleRoundWithDice(initialState, attackerDice, defenderDice)
        
        val attackerLosses = attackingTroops - afterRound.attackingTroops
        val defenderLosses = defenderTerritory.troops - afterRound.defendingTroops
        
        val battleResult = 
          if (afterRound.defendingTroops == 0)
            BattleResult.AttackerWins
          else if (afterRound.attackingTroops == 0)
            BattleResult.DefenderWins
          else
            BattleResult.BattleContinues
            
        val (updatedAttacker, updatedDefender) = battleResult match
          case BattleResult.AttackerWins =>
            val conqueredTerritory = defenderTerritory
              .changeOwner(attacker)
              .copy(troops = afterRound.attackingTroops)
            val updatedAttackerTerritory = attackerTerritory
              .copy(troops = attackerTerritory.troops - attackingTroops)
            (updatedAttackerTerritory, conqueredTerritory)
            
          case BattleResult.DefenderWins =>
            val updatedAttackerTerritory = attackerTerritory
              .copy(troops = attackerTerritory.troops - attackingTroops)
            (updatedAttackerTerritory, defenderTerritory)
            
          case BattleResult.BattleContinues =>
            val updatedAttackerTerritory = attackerTerritory
              .addTroops(-attackerLosses)
            val updatedDefenderTerritory = defenderTerritory
              .addTroops(-defenderLosses)
            (updatedAttackerTerritory, updatedDefenderTerritory)
            
        BattleRoundResult(
          updatedAttacker,
          updatedDefender,
          attackerLosses,
          defenderLosses,
          battleResult,
          attackerDice,
          defenderDice
        )