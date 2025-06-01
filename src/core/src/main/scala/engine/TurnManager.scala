package engine
import model.cards.*
import model.player.*
import exceptions.*

trait TurnManager:
    def currentPlayer: Player
    def nextPlayer(): TurnManager
    def currentPhase: TurnPhase
    def nextPhase(): TurnManager
    def isValidAction(action: GameAction): Boolean

case class TurnManagerImpl(
    players: List[Player],
    currentPlayerIndex: Int = 0,
    phase: TurnPhase = TurnPhase.WaitingForTurn
) extends TurnManager:

    def currentPlayer: Player = players match
        case Nil => throw InvalidPlayerException()
        case _ => players(currentPlayerIndex)

    def nextPlayer(): TurnManager = players match
        case Nil => throw InvalidPlayerException()
        case _ => 
            // if nextIndex grows beyond the list size, it wraps around
            val nextIndex = (currentPlayerIndex + 1) % players.size
            copy(currentPlayerIndex = nextIndex, phase = TurnPhase.WaitingForTurn)

    def currentPhase: TurnPhase = phase

    def nextPhase(): TurnManager = phase match
        case TurnPhase.WaitingForTurn => copy(phase = TurnPhase.PlacingTroops)
        case TurnPhase.PlacingTroops => copy(phase = TurnPhase.Reinforcement)
        case TurnPhase.Reinforcement => copy(phase = TurnPhase.Attacking)
        case TurnPhase.Attacking => copy(phase = TurnPhase.Defending)
        case TurnPhase.Defending => copy(phase = TurnPhase.WaitingForTurn)
        case _ => throw InvalidPhaseTransitionException()

    def isValidAction(action: GameAction): Boolean = (action, phase) match
        case (GameAction.PlaceTroops(playerId, troops), TurnPhase.PlacingTroops) => 
            playerId == currentPlayer.id && troops > 0
            
        case (GameAction.Reinforce(playerId, troops), TurnPhase.Reinforcement) => 
            playerId == currentPlayer.id && troops > 0
            
        case (GameAction.TradeCards(_), TurnPhase.Reinforcement) => true
            
        case (GameAction.Attack(attackerId, defenderId), TurnPhase.Attacking) => 
            attackerId == currentPlayer.id && attackerId != defenderId
            
        case (GameAction.Defend(defenderId, troops), TurnPhase.Defending) => 
            defenderId != currentPlayer.id && troops > 0 && troops <= 3
            
        case (GameAction.EndAttack, TurnPhase.Attacking) => true
        case (GameAction.EndPhase, _) => true
        case (GameAction.EndTurn, _) => true
        case _ => false
