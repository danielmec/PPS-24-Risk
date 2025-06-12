package engine

import model.player.*
import model.cards.*
import model.board.*
import exceptions._
import utils.*
import utils.GameEngineUtils.*

case class EngineState(
  gameState: GameState,
  pendingAttack: Option[(PlayerImpl, PlayerImpl, Territory, Territory, Int)] = None,
  territoryConqueredThisTurn: Boolean = false,
  strategicMoveUsed: Boolean = false
)

class GameEngine(
    val players: List[PlayerImpl],
    val gameId: String = java.util.UUID.randomUUID().toString
):
  private val (continents, territoriesMap) = CardsBuilder.createBoard()
  private val board = Board(gameId, continents)
  private val territoryDeck = CardsBuilder.createTerritoriesDeck()
  private val objectiveDeck = CardsBuilder.createObjectivesDeck()

  private val playerStates: List[PlayerState] = players.map: p =>
    PlayerState(p, Set.empty, None, TurnPhase.WaitingForTurn, 0)

  private val turnManager: TurnManager = TurnManagerImpl(players)
  private val decksManager: DecksManager = DecksManagerImpl(territoryDeck, objectiveDeck)

  private var state: EngineState = EngineState(
    GameState(
      gameId = gameId,
      board = board,
      playerStates = playerStates,
      turnManager = turnManager,
      decksManager = decksManager
    )
  )

  /** Executes a game action and updates the state */
  def performAction(action: GameAction): Either[String, GameState] =
    performActionFunctional(state, action).map { newState =>
      state = newState
      newState.gameState
    }

  private def performActionFunctional(
    state: EngineState,
    action: GameAction
  ): Either[String, EngineState] =
    val gameState = state.gameState
    action match
      case GameAction.PlaceTroops(playerId, troops, territoryName) =>
        for
          territory <- gameState.board.territories.find(_.name == territoryName).toRight("Invalid territory or not owned by player.")
          playerState <- gameState.playerStates.find(_.playerId == playerId).toRight("Invalid territory or not owned by player.")
          _ <-
            if !territory.owner.exists(_.id == playerId) then Left("Invalid territory or not owned by player.")
            else if troops <= 0 || troops > playerState.bonusTroops then Left(s"Invalid number of troops to place. You can place at most ${playerState.bonusTroops} troops.")
            else Right(())
          updatedTerritory = territory.copy(troops = territory.troops + troops)
          updatedBoard = gameState.board.updatedTerritory(updatedTerritory)
          updatedPlayerState = playerState.copy(bonusTroops = playerState.bonusTroops - troops)
          updatedPlayerStates = gameState.playerStates.map:
            case ps if ps.playerId == playerId => updatedPlayerState
            case ps => ps
          newGameState = gameState.updateBoard(updatedBoard).copy(playerStates = updatedPlayerStates)
        yield state.copy(gameState = newGameState)

      case GameAction.Reinforce(playerId, from, to, troops) =>
        if gameState.turnManager.currentPhase == TurnPhase.Reinforcement && state.strategicMoveUsed then
          Left("Strategic move already used this turn.")
        else
          for
            fromTerritory <- gameState.board.territories.find(_.name == from).toRight("Invalid territories, not owned, not adjacent or invalid number of troops.")
            toTerritory <- gameState.board.territories.find(_.name == to).toRight("Invalid territories, not owned, not adjacent or invalid number of troops.")
            _ <-
              if !fromTerritory.owner.exists(_.id == playerId) || !toTerritory.owner.exists(_.id == playerId) then Left("Invalid territories, not owned, not adjacent or invalid number of troops.")
              else if fromTerritory.troops <= troops || troops <= 0 then Left("Invalid territories, not owned, not adjacent or invalid number of troops.")
              else if !fromTerritory.neighbors.exists(_.name == toTerritory.name) then Left("Invalid territories, not owned, not adjacent or invalid number of troops.")
              else Right(())
            updatedFrom = fromTerritory.copy(troops = fromTerritory.troops - troops)
            updatedTo = toTerritory.copy(troops = toTerritory.troops + troops)
            updatedBoard = gameState.board.updatedTerritory(updatedFrom).updatedTerritory(updatedTo)
            newGameState = gameState.updateBoard(updatedBoard)
            newStrategicMoveUsed = if gameState.turnManager.currentPhase == TurnPhase.Reinforcement then true else state.strategicMoveUsed
          yield state.copy(gameState = newGameState, strategicMoveUsed = newStrategicMoveUsed)

      case GameAction.Attack(attackerId, defenderId, from, to, troops) =>
        for
          attacker <- players.find(_.id == attackerId).toRight("Invalid territories or players for attack.")
          defender <- players.find(_.id == defenderId).toRight("Invalid territories or players for attack.")
          attackerTerritory <- gameState.board.territories.find(_.name == from).toRight("Invalid territories or players for attack.")
          defenderTerritory <- gameState.board.territories.find(_.name == to).toRight("Invalid territories or players for attack.")
          _ <-
            if troops <= 0 || troops >= attackerTerritory.troops then Left("Invalid number of troops for attack.")
            else if !attackerTerritory.owner.contains(attacker) || !defenderTerritory.owner.contains(defender) then Left("Territories are not owned by the correct players.")
            else Right(())
        yield state.copy(pendingAttack = Some((attacker, defender, attackerTerritory, defenderTerritory, troops)))

      case GameAction.Defend(defenderId, territoryName, defendTroops) =>
        state.pendingAttack match
          case Some((attacker, defender, attackerTerritory, defenderTerritory, attackingTroops))
            if defender.id == defenderId && defenderTerritory.name == territoryName =>
              val maxDefend = math.min(3, defenderTerritory.troops)
              if defendTroops <= 0 || defendTroops > maxDefend then
                Left(s"Invalid number of defending troops (max: $maxDefend).")
              else
                val defenderDiceRoll: Int => Seq[Int] = _ => utils.Dice.roll(defendTroops)
                val (result, updatedAttackerTerritory, updatedDefenderTerritory) =
                  Battle.battle(
                    attacker,
                    defender,
                    attackerTerritory,
                    defenderTerritory,
                    attackingTroops,
                    attackerDiceRoll = utils.Dice.roll,
                    defenderDiceRoll = defenderDiceRoll
                  )
                val conquered = updatedDefenderTerritory.owner.exists(_.id == attacker.id)
                val updatedBoard = gameState.board
                  .updatedTerritory(updatedAttackerTerritory)
                  .updatedTerritory(updatedDefenderTerritory)
                val afterElimination =
                  if !hasRemainingTerritories(gameState.updateBoard(updatedBoard), defender.id)
                  then transferCardsOnElimination(gameState.updateBoard(updatedBoard), defender.id, attacker.id)
                  else gameState.updateBoard(updatedBoard)
                Right(state.copy(
                  gameState = afterElimination,
                  pendingAttack = None,
                  territoryConqueredThisTurn = state.territoryConqueredThisTurn || conquered
                ))
          case _ =>
            Left("No pending attack or invalid defender data.")

      case GameAction.TradeCards(cards) =>
        val currentPlayerId = gameState.turnManager.currentPlayer.id
        gameState.playerStates.find(_.playerId == currentPlayerId) match
          case Some(playerState) =>
            if cards.size != 3 then
              Left("You must trade exactly 3 territory cards.")
            else if !cards.subsetOf(playerState.territoryCards) then
              Left("You don't own all the territory cards you want to trade.")
            else
              val bonus = BonusCalculator.calculateTradeBonus(cards)
              if bonus == 0 then
                Left("Invalid card combination for bonus.")
              else
                val updatedPlayerState = playerState
                  .removeTerritoryCards(cards)
                  .copy(bonusTroops = playerState.bonusTroops + bonus)
                val updatedPlayerStates = gameState.playerStates.map:
                  case ps if ps.playerId == currentPlayerId => updatedPlayerState
                  case ps => ps
                val newGameState = gameState.copy(playerStates = updatedPlayerStates)
                Right(state.copy(gameState = newGameState))
          case None =>
            Left("Player not found.")

      case GameAction.EndAttack | GameAction.EndPhase | GameAction.EndTurn =>
        val isEndTurn = action == GameAction.EndTurn
        val (afterCardDraw, afterDecksManager, afterConquered) =
          if isEndTurn && state.territoryConqueredThisTurn then
            val currentPlayerId = gameState.turnManager.currentPlayer.id
            val (updatedGameState, updatedDecksManager) = drawTerritoryCard(gameState, gameState.decksManager, currentPlayerId)
            (updatedGameState, updatedDecksManager, false)
          else (gameState, gameState.decksManager, state.territoryConqueredThisTurn)
        val nextTurnManager = afterCardDraw.turnManager.nextPhase()
        val afterBonus =
          if nextTurnManager.currentPhase == TurnPhase.PlacingTroops then
            val currentPlayerId = nextTurnManager.currentPlayer.id
            val bonus = BonusCalculator.calculateStartTurnBonus(currentPlayerId, afterCardDraw.board)
            val updatedPlayerStates = afterCardDraw.playerStates.map:
              case ps if ps.playerId == currentPlayerId =>
                ps.copy(bonusTroops = ps.bonusTroops + bonus)
              case ps => ps
            afterCardDraw.copy(playerStates = updatedPlayerStates)
          else afterCardDraw
        val afterTurnManager = afterBonus.updateTurnManager(nextTurnManager).copy(decksManager = afterDecksManager)
        val newState = state.copy(
          gameState = afterTurnManager,
          strategicMoveUsed = if isEndTurn then false else state.strategicMoveUsed,
          territoryConqueredThisTurn = afterConquered
        )
        checkVictory(newState) match
          case Some(winner) => Left(s"Game over! The winner is ${winner.player.name}!")
          case None => Right(newState)

  def setGameState(newState: GameState): Unit =
    state = state.copy(gameState = newState)

  def getGameState: GameState = state.gameState

  def checkVictory: Option[PlayerState] =
    state.gameState.checkWinCondition

  private def checkVictory(state: EngineState): Option[PlayerState] =
    state.gameState.checkWinCondition

