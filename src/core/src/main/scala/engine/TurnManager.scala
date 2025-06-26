package engine
import model.cards._
import model.player._
import model.board._
import utils._
import exceptions._

trait TurnManager:
    def currentPlayer: Player
    def nextPlayer(): TurnManager
    def currentPhase: TurnPhase
    def nextPhase(): TurnManager
    def isValidAction(action: GameAction, gameState: GameState, engineState: EngineState): Boolean

case class TurnManagerImpl(
    players: List[Player],
    currentPlayerIndex: Int = 0,
    phase: TurnPhase = TurnPhase.SetupPlacing
) extends TurnManager:

    def currentPlayer: Player = players match
        case Nil => throw InvalidPlayerException()
        case _ => players(currentPlayerIndex)

    def nextPlayer(): TurnManager = players match
        case Nil => throw InvalidPlayerException()
        case _ => 
            // if nextIndex grows beyond the list size, it wraps around
            val nextIndex = (currentPlayerIndex + 1) % players.size
            if (phase == TurnPhase.SetupPlacing && nextIndex != 0)
                copy(currentPlayerIndex = nextIndex, phase = TurnPhase.SetupPlacing)
            else
                copy(currentPlayerIndex = nextIndex, phase = TurnPhase.PlacingTroops)

    def currentPhase: TurnPhase = phase

    def nextPhase(): TurnManager = phase match
        case TurnPhase.SetupPlacing => copy(phase = TurnPhase.SetupPlacing)
        case TurnPhase.WaitingForTurn => copy(phase = TurnPhase.PlacingTroops)
        case TurnPhase.PlacingTroops => copy(phase = TurnPhase.Attacking)
        case TurnPhase.Reinforcement => copy(phase = TurnPhase.Attacking)
        case TurnPhase.Attacking => copy(phase = TurnPhase.Defending)
        case TurnPhase.Defending => copy(phase = TurnPhase.WaitingForTurn)

    def isValidAction(action: GameAction, gameState: GameState, engineState: EngineState): Boolean = (action, phase) match
        case (GameAction.PlaceTroops(playerId, troops, territoryName), (TurnPhase.SetupPlacing | TurnPhase.PlacingTroops)) => 
            val playerState = gameState.getPlayerState(playerId).getOrElse(throw new InvalidActionException())
            val territory = gameState.getTerritoryByName(territoryName).getOrElse(throw new InvalidActionException())
            playerId == currentPlayer.id && 
            troops > 0 &&
            territory.isOwnedBy(playerId)
            
        case (GameAction.Reinforce(playerId, from, to, troops), TurnPhase.Reinforcement) => 
            val fromTerritory = gameState.getTerritoryByName(from).getOrElse(throw new InvalidActionException())
            val toTerritory = gameState.getTerritoryByName(to).getOrElse(throw new InvalidActionException())
            playerId == currentPlayer.id &&
            fromTerritory.isOwnedBy(playerId) && 
            toTerritory.isOwnedBy(playerId) &&
            fromTerritory.hasEnoughTroops(troops + 1) &&
            gameState.board.areNeighbors(fromTerritory, toTerritory) &&
            troops > 0
            
        case (GameAction.TradeCards(territoryCards), TurnPhase.Reinforcement) => 
            val playerState = gameState.getPlayerState(currentPlayer.id).getOrElse(throw new InvalidActionException())  
            territoryCards.size == 3 && 
            territoryCards.subsetOf(playerState.territoryCards) &&
            BonusCalculator.calculateTradeBonus(territoryCards) > 0
            
        case (GameAction.Attack(attackerId, defenderId, from, to, numTroops), TurnPhase.Attacking) =>
            val fromTerritory = gameState.getTerritoryByName(from).getOrElse(throw new InvalidActionException())
            val toTerritory = gameState.getTerritoryByName(to).getOrElse(throw new InvalidActionException())
            attackerId == currentPlayer.id && 
            attackerId != defenderId && 
            numTroops > 0 &&
            fromTerritory.isOwnedBy(attackerId) && 
            toTerritory.isOwnedBy(defenderId) &&
            fromTerritory.hasEnoughTroops(numTroops + 1) &&
            gameState.board.areNeighbors(fromTerritory, toTerritory)
            
        case (GameAction.Defend(defenderId, territory, troops), TurnPhase.Defending) => 
            engineState.pendingAttack match
                case Some((_, defender, _, defenderTerritory, _)) =>
                    defenderId != currentPlayer.id &&
                    defender.id == defenderId && 
                    defenderTerritory.name == territory &&
                    troops <= defenderTerritory.troops && 
                    troops > 0 &&
                    troops <= 3
                case None => false
            
        case (GameAction.EndAttack, TurnPhase.Attacking) => true
        case (GameAction.EndPhase, _) => true
        case (GameAction.EndTurn, _) => true
        case (GameAction.EndSetup, _) => true
        case _ => false
