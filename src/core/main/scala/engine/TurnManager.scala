import engine.TurnPhase
import engine.player.Player
import exceptions.InvalidPlayerException
import engine.GameAction

trait TurnManager:
    def currentPlayer: Player, 
    def nextPlayer(): TurnManager,
    def currentPhase: TurnPhase,
    def nextPhase(): TurnManager,
    def isValidAction(action: GameAction): Boolean

case class TurnManagerImpl(
    players: List[Player],
    currentPlayerIndex: Int = 0,
    phase: TurnPhase = TurnPhase.WaitingForTurn
) extends TurnManager:

    def currentPlayer: Player = players match
        case Nil => throw InvalidPlayerException()
        case _ => players(currentPlayerIndex)

    def nextPlayer: TurnManager = players match
        case Nil => throw InvalidPlayerException()
        case _ => 
            // if nextIndex grows beyond the list size, it wraps around
            val nextIndex = (currentPlayerIndex + 1) % players.size
            copy(currentPlayerIndex = nextIndex, phase = TurnPhase.WaitingForTurn)

    def currentPhase: TurnPhase = phase

    def nextPhase: TurnManager = phase match
        case TurnPhase.WaitingForTurn => copy(phase = TurnPhase.PlacingTroops)
        case TurnPhase.PlacingTroops => copy(phase = TurnPhase.Reinforcement)
        case TurnPhase.Reinforcement => copy(phase = TurnPhase.Attacking)
        case TurnPhase.Attacking => copy(phase = TurnPhase.Defending)
        case TurnPhase.Defending => copy(phase = TurnPhase.WaitingForTurn)
        case _ => throw InvalidPhaseTransitionException()

    def isValidAction(action: GameAction): Boolean = (action, phase) match
        case (GameAction.PlaceTroops(_), TurnPhase.PlacingTroops) => true
        case (GameAction.Reinforce(_), TurnPhase.Reinforcement) => true
        case (GameAction.Attack(_, _), TurnPhase.Attacking) => true
        case (GameAction.Defend(_, _), TurnPhase.Defending) => true
        case _ => false
