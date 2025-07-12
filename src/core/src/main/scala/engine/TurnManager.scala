package engine

import model.cards._
import model.player._
import model.board._
import utils._
import exceptions._

trait TurnManager:
    /**
      * Returns the player whose turn is currently active.
      * @return the current Player
      */
    def currentPlayer: Player

    /**
      * Advances to the next player, updating the phase if necessary.
      * @return a new TurnManager reflecting the updated state
      */
    def nextPlayer(): TurnManager

    /**
      * Returns the current phase of the game (SetupPhase or MainPhase).
      * @return the current TurnPhase
      */
    def currentPhase: TurnPhase

    /**
      * Checks if a given action is valid in the current game state and phase.
      * @param action the action to validate
      * @param gameState the current state of the game
      * @param engineState the current state of the engine
      * @return true if the action is valid, false otherwise
      */
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
            val nextIndex = (currentPlayerIndex + 1) % players.size
            if (phase == TurnPhase.SetupPhase && nextIndex != 0)
                copy(currentPlayerIndex = nextIndex, phase = TurnPhase.SetupPhase)
            else
                copy(currentPlayerIndex = nextIndex, phase = TurnPhase.MainPhase)

    def currentPhase: TurnPhase = phase

    def isValidAction(action: GameAction, gameState: GameState, engineState: EngineState): Boolean =

      def getPlayerStateOrThrow(id: String) =
        gameState.getPlayerState(id).getOrElse(throw new InvalidActionException())

      def getTerritoryOrThrow(name: String) =
        gameState.getTerritoryByName(name).getOrElse(throw new InvalidActionException())

      (action, phase) match
        case (GameAction.PlaceTroops(playerId, troops, territoryName), _) =>
          val playerState = getPlayerStateOrThrow(playerId)
          val territory = getTerritoryOrThrow(territoryName)
          val phaseValidation = phase match
            case TurnPhase.SetupPhase => true
            case TurnPhase.MainPhase  => playerState.bonusTroops >= troops
          playerId == currentPlayer.id &&
          troops > 0 &&
          territory.isOwnedBy(playerId) &&
          phaseValidation

        case (GameAction.Reinforce(playerId, from, to, numTroops), TurnPhase.MainPhase) =>
          val playerState = getPlayerStateOrThrow(playerId)
          val fromTerritory = getTerritoryOrThrow(from)
          val toTerritory = getTerritoryOrThrow(to)
          playerId == currentPlayer.id &&
          playerState.bonusTroops == 0 &&
          fromTerritory.isOwnedBy(playerId) &&
          toTerritory.isOwnedBy(playerId) &&
          fromTerritory.hasEnoughTroops(numTroops + 1) &&
          gameState.board.areNeighbors(fromTerritory, toTerritory)

        case (GameAction.Attack(attackerId, defenderId, from, to, numTroops), TurnPhase.MainPhase) =>
          val playerState = getPlayerStateOrThrow(attackerId)
          val fromTerritory = getTerritoryOrThrow(from)
          val toTerritory = getTerritoryOrThrow(to)
          attackerId == currentPlayer.id &&
          attackerId != defenderId &&
          playerState.bonusTroops == 0 &&
          numTroops > 0 &&
          fromTerritory.isOwnedBy(attackerId) &&
          toTerritory.isOwnedBy(defenderId) &&
          fromTerritory.hasEnoughTroops(numTroops + 1) &&
          gameState.board.areNeighbors(fromTerritory, toTerritory)

        case (GameAction.TradeCards(playerId, cardNames), TurnPhase.MainPhase) =>
          val playerState = getPlayerStateOrThrow(playerId)
          val cardNameCounts = cardNames.groupBy(identity).view.mapValues(_.size).toMap
          val selectedCards: Seq[TerritoryCard] = playerState.territoryCards
            .groupBy(_.territory.name)
            .flatMap { case (name, cards) =>
              val count = cardNameCounts.getOrElse(name, 0)
              cards.take(count)
            }.toSeq
          selectedCards.size == 3 &&
          selectedCards.forall(playerState.territoryCards.contains) &&
          BonusCalculator.calculateTradeBonus(selectedCards) > 0

        case (GameAction.EndTurn | GameAction.EndSetup, _) if phase == TurnPhase.MainPhase || phase == TurnPhase.SetupPhase =>
          val playerState = getPlayerStateOrThrow(currentPlayer.id)
          playerState.bonusTroops == 0

        case _ => false