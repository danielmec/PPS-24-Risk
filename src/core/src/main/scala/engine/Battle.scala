package engine

import model.board.Territory
import model.player.Player
import utils.Dice
import exceptions._

object BattleResult extends Enumeration:
  type BattleResult = Value
  val AttackerWins, DefenderWins, BattleContinues = Value // Aggiunto BattleContinues

import BattleResult._

// Stato immutabile della battaglia
case class BattleState(
  attacker: Player,
  defender: Player,
  attackerTerritory: Territory,
  defenderTerritory: Territory,
  attackingTroops: Int,
  defendingTroops: Int,
)

// Risultato di un singolo round
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

  /**
   * Esegue un singolo round di combattimento
   */
  def resolveBattleRound(state: BattleState, attackerDice : Seq[Int], defenderDice : Seq[Int]): BattleState =
    
    // Confrontiamo i dadi accoppiati
    val pairs = attackerDice.zip(defenderDice)
    val (attackerLosses, defenderLosses) = pairs.foldLeft((0, 0)):
      case ((aLoss, dLoss), (aDice, dDice)) =>
        if (aDice > dDice) (aLoss, dLoss + 1)
        else (aLoss + 1, dLoss)

    state.copy(
      attackingTroops = state.attackingTroops - attackerLosses,
      defendingTroops = state.defendingTroops - defenderLosses
    )

  /**
   * Valida se un attacco può essere eseguito
   */
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

  /**
   * Esegue un singolo round di combattimento e restituisce il risultato
   */
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

        // L'attaccante usa tante truppe quante ne ha deciso all'inizio (max 3)
        val attackerDiceCount = math.min(3, initialState.attackingTroops)
        // Il difensore usa sempre il massimo (max 3)
        val defenderDiceCount = math.min(3, initialState.defendingTroops)
        
        // Lanciamo i dadi e ordiniamoli
        val attackerDice = Dice.rollMany(attackerDiceCount)
        val defenderDice = Dice.rollMany(defenderDiceCount)
        
        val afterRound = resolveBattleRound(initialState, attackerDice, defenderDice)
        
        // Calcola le perdite
        val attackerLosses = attackingTroops - afterRound.attackingTroops
        val defenderLosses = defenderTerritory.troops - afterRound.defendingTroops
        
        // Determina il risultato
        val battleResult = 
          if (afterRound.defendingTroops == 0)
            BattleResult.AttackerWins
          else if (afterRound.attackingTroops == 0)
            BattleResult.DefenderWins
          else
            BattleResult.BattleContinues
            
        // Aggiorna i territori
        val (updatedAttacker, updatedDefender) = battleResult match
          case BattleResult.AttackerWins =>
            // Difensore sconfitto, l'attaccante conquista il territorio
            val conqueredTerritory = defenderTerritory
              .changeOwner(attacker)
              .copy(troops = afterRound.attackingTroops)
            val updatedAttackerTerritory = attackerTerritory
              .copy(troops = attackerTerritory.troops - attackingTroops)
            (updatedAttackerTerritory, conqueredTerritory)
            
          case BattleResult.DefenderWins =>
            // Attaccante sconfitto
            val updatedAttackerTerritory = attackerTerritory
              .copy(troops = attackerTerritory.troops - attackingTroops)
            (updatedAttackerTerritory, defenderTerritory)
            
          case BattleResult.BattleContinues =>
            // La battaglia può continuare
            val updatedAttackerTerritory = attackerTerritory
              .copy(troops = attackerTerritory.troops - attackerLosses)
            val updatedDefenderTerritory = defenderTerritory
              .copy(troops = defenderTerritory.troops - defenderLosses)
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