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
    def isValidAction(action: GameAction, gameState: GameState, engineState: EngineState): Boolean

case class TurnManagerImpl(
    players: List[Player],
    currentPlayerIndex: Int = 0,
    phase: TurnPhase = TurnPhase.SetupPhase
) extends TurnManager:

    def currentPlayer: Player = players match
        case Nil => throw InvalidPlayerException()
        case _ => players(currentPlayerIndex)

    def nextPlayer(): TurnManager = players match
        case Nil => throw InvalidPlayerException()
        case _ => 
            // if nextIndex grows beyond the list size, it wraps around
            val nextIndex = (currentPlayerIndex + 1) % players.size
            if (phase == TurnPhase.SetupPhase && nextIndex != 0)
                // Durante il setup, rimaniamo in SetupPhase
                println("HEY HEY SONO PROPRIO QUI")
                copy(currentPlayerIndex = nextIndex, phase = TurnPhase.SetupPhase)
            else
                // Dopo il setup o quando il turno torna al primo giocatore, passiamo alla MainPhase
                println("HEY HEY MI SONO PROPRIO SPOSTATO")
                copy(currentPlayerIndex = nextIndex, phase = TurnPhase.MainPhase)

    def currentPhase: TurnPhase = phase

    def isValidAction(action: GameAction, gameState: GameState, engineState: EngineState): Boolean = (action, phase) match
        // Piazzamento truppe consentito in entrambe le fasi
        case (GameAction.PlaceTroops(playerId, troops, territoryName), _) => 
            val playerState = gameState.getPlayerState(playerId).getOrElse(throw new InvalidActionException())
            val territory = gameState.getTerritoryByName(territoryName).getOrElse(throw new InvalidActionException())
            playerId == currentPlayer.id && 
            troops > 0 &&
            territory.isOwnedBy(playerId) &&
            (phase == TurnPhase.SetupPhase || playerState.bonusTroops >= troops)
            
        // Rinforzo consentito solo in MainPhase
        case (GameAction.Reinforce(playerId, from, to, numTroops), TurnPhase.MainPhase) =>
          val playerState = gameState.getPlayerState(playerId).getOrElse(throw new InvalidActionException())
          val fromTerritory = gameState.getTerritoryByName(from).getOrElse(throw new InvalidActionException())
          val toTerritory = gameState.getTerritoryByName(to).getOrElse(throw new InvalidActionException())
          playerId == currentPlayer.id &&
          playerState.bonusTroops == 0 &&
          fromTerritory.isOwnedBy(playerId) &&
          toTerritory.isOwnedBy(playerId) &&
          fromTerritory.hasEnoughTroops(numTroops + 1) &&
          gameState.board.areNeighbors(fromTerritory, toTerritory)
            
        // Attacco consentito solo in MainPhase
        case (GameAction.Attack(attackerId, defenderId, from, to, numTroops), TurnPhase.MainPhase) =>
            val playerState = gameState.getPlayerState(attackerId).getOrElse(throw new InvalidActionException())
            val fromTerritory = gameState.getTerritoryByName(from).getOrElse(throw new InvalidActionException())
            val toTerritory = gameState.getTerritoryByName(to).getOrElse(throw new InvalidActionException())
            attackerId == currentPlayer.id && 
            attackerId != defenderId && 
            playerState.bonusTroops == 0 && // Il giocatore deve aver piazzato tutte le truppe bonus
            numTroops > 0 &&
            fromTerritory.isOwnedBy(attackerId) && 
            toTerritory.isOwnedBy(defenderId) &&
            fromTerritory.hasEnoughTroops(numTroops + 1) &&
            gameState.board.areNeighbors(fromTerritory, toTerritory)
            
        // Scambio carte consentito solo in MainPhase
        case (GameAction.TradeCards(territoryCards), TurnPhase.MainPhase) => 
            val playerState = gameState.getPlayerState(currentPlayer.id).getOrElse(throw new InvalidActionException())  
            territoryCards.size == 3 && 
            territoryCards.subsetOf(playerState.territoryCards) &&
            BonusCalculator.calculateTradeBonus(territoryCards) > 0
        
        // EndSetup è valido solo in SetupPhase
        case (GameAction.EndSetup, TurnPhase.SetupPhase) => true
        
        // EndTurn sempre valido
        case (GameAction.EndTurn, _) => true
        
        // Altri casi non validi
        case _ => false
