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

  def initGame(action: GameAction): GameState =
    try {
      val newState = performActions(state, action)
      state = newState
      newState.gameState
    } catch {
      case e: Exception => throw e
    }

  /** Executes a game action and updates the state */
  def performActions(state: EngineState, action: GameAction): EngineState =
    val gameState = state.gameState
    
    val gameStateWithBonus = 
      if gameState.turnManager.currentPhase == TurnPhase.PlacingTroops then
        val currentPlayerId = gameState.turnManager.currentPlayer.id
        val playerState = gameState.playerStates.find(_.playerId == currentPlayerId).get
        
        if playerState.bonusTroops == 0 then
          val bonus = BonusCalculator.calculateStartTurnBonus(currentPlayerId, gameState.board)
          val updatedPlayerStates = gameState.playerStates.map:
            case ps if ps.playerId == currentPlayerId =>
              ps.copy(bonusTroops = ps.bonusTroops + bonus)
            case ps => ps
          gameState.copy(playerStates = updatedPlayerStates)
        else gameState
      else gameState
    
    val updatedState = state.copy(gameState = gameStateWithBonus)
    
    action match
      case GameAction.PlaceTroops(playerId, troops, territoryName) => 
        placeTroopsAction(gameStateWithBonus, playerId, updatedState, territoryName, troops)
      case GameAction.Reinforce(playerId, from, to, troops) => reinforceAction(from, gameState, playerId, state, to, troops)
      case GameAction.Attack(attackerId, defenderId, from, to, troops) => attackAction(attackerId, defenderId, from, gameState, state, to, troops)
      case GameAction.Defend(defenderId, territoryName, defendTroops) => defendAction(defendTroops, defenderId, gameState, state, territoryName)
      case GameAction.TradeCards(cards) => tradeCardsAction(cards, gameState, state)
      case GameAction.EndAttack | GameAction.EndPhase | GameAction.EndTurn => endAction(state, action)


  private def placeTroopsAction(gameState: GameState, playerId: String, state: EngineState, territoryName: String, troops: Int): EngineState =
    val territory = gameState.board.territories.find(_.name == territoryName).getOrElse(throw new InvalidTerritoryException())
    val playerState = gameState.playerStates.find(_.playerId == playerId).getOrElse(throw new InvalidPlayerException())
    if !territory.owner.exists(_.id == playerId) then throw new InvalidTerritoryException()
    if troops <= 0 || troops > playerState.bonusTroops then throw new InvalidTroopsException()
    val updatedTerritory = territory.copy(troops = territory.troops + troops)
    val updatedBoard = gameState.board.updatedTerritory(updatedTerritory)
    val updatedPlayerState = playerState.copy(bonusTroops = playerState.bonusTroops - troops)
    val updatedPlayerStates = gameState.playerStates.map:
      case playerState if playerState.playerId == playerId => updatedPlayerState
      case playerState => playerState
    val newGameState = gameState.updateBoard(updatedBoard).copy(playerStates = updatedPlayerStates)
    state.copy(gameState = newGameState)

  private def reinforceAction(from: String, gameState: GameState, playerId: String, state: EngineState, to: String, troops: Int): EngineState =
    if (gameState.turnManager.currentPhase == TurnPhase.Reinforcement && state.strategicMoveUsed) then throw new InvalidActionException()
    val fromTerritory = gameState.board.territories.find(_.name == from).getOrElse(throw new InvalidTerritoryException())
    val toTerritory = gameState.board.territories.find(_.name == to).getOrElse(throw new InvalidTerritoryException())
    if (!fromTerritory.owner.exists(_.id == playerId) || !toTerritory.owner.exists(_.id == playerId)) then throw new InvalidTerritoryException()
    if (fromTerritory.troops <= troops || troops <= 0) then throw new InvalidTroopsException()
    if !fromTerritory.neighbors.exists(_.name == toTerritory.name) then throw new InvalidTerritoryException()
    val updatedFrom = fromTerritory.copy(troops = fromTerritory.troops - troops)
    val updatedTo = toTerritory.copy(troops = toTerritory.troops + troops)
    val updatedBoard = gameState.board.updatedTerritory(updatedFrom).updatedTerritory(updatedTo)
    val newGameState = gameState.updateBoard(updatedBoard)
    val newStrategicMoveUsed = if gameState.turnManager.currentPhase == TurnPhase.Reinforcement then true else state.strategicMoveUsed
    state.copy(gameState = newGameState, strategicMoveUsed = newStrategicMoveUsed)

  private def attackAction(attackerId: String, defenderId: String, from: String, gameState: GameState, state: EngineState, to: String, troops: Int): EngineState =
    val attacker = players.find(_.id == attackerId).getOrElse(throw new InvalidPlayerException())
    val defender = players.find(_.id == defenderId).getOrElse(throw new InvalidPlayerException())
    val attackerTerritory = gameState.board.territories.find(_.name == from).getOrElse(throw new InvalidTerritoryException())
    val defenderTerritory = gameState.board.territories.find(_.name == to).getOrElse(throw new InvalidTerritoryException())
    if (troops <= 0 || troops >= attackerTerritory.troops) then throw new InvalidTroopsException()
    if (!attackerTerritory.owner.contains(attacker) || !defenderTerritory.owner.contains(defender)) then throw new InvalidTerritoryException()
    state.copy(pendingAttack = Some((attacker, defender, attackerTerritory, defenderTerritory, troops)))

  private def defendAction(defendTroops: Int, defenderId: String, gameState: GameState, state: EngineState, territoryName: String): EngineState =
    state.pendingAttack match
      case Some((attacker, defender, attackerTerritory, defenderTerritory, attackingTroops))
        if defender.id == defenderId && defenderTerritory.name == territoryName =>
          val maxDefend = math.min(3, defenderTerritory.troops)
          if (defendTroops <= 0 || defendTroops > maxDefend) then throw new InvalidActionException() 
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
          val updatedBoard = gameState.board.updatedTerritory(updatedAttackerTerritory).updatedTerritory(updatedDefenderTerritory)
          val afterElimination =
            if !hasRemainingTerritories(gameState.updateBoard(updatedBoard), defender.id)
            then transferCardsOnElimination(gameState.updateBoard(updatedBoard), defender.id, attacker.id)
            else gameState.updateBoard(updatedBoard)
          state.copy(
            gameState = afterElimination,
            pendingAttack = None,
            territoryConqueredThisTurn = state.territoryConqueredThisTurn || conquered
          )        
      case _ => throw new InvalidActionException()

  private def tradeCardsAction(cards: Set[TerritoryCard], gameState: GameState, state: EngineState): EngineState =
    val currentPlayerId = gameState.turnManager.currentPlayer.id
    val playerState = gameState.playerStates.find(_.playerId == currentPlayerId).getOrElse(throw new InvalidPlayerException())
    if cards.size != 3 then throw new InvalidCardException()
    if !cards.subsetOf(playerState.territoryCards) then throw new InvalidCardException()     
    val bonus = BonusCalculator.calculateTradeBonus(cards)     
    if bonus == 0 then throw new InvalidCardException()   
    val updatedPlayerState = playerState.removeTerritoryCards(cards).copy(bonusTroops = playerState.bonusTroops + bonus)   
    val updatedPlayerStates = gameState.playerStates.map:
      case playerState if (playerState.playerId == currentPlayerId) => updatedPlayerState
      case playerState => playerState         
    val newGameState = gameState.copy(playerStates = updatedPlayerStates)
    state.copy(gameState = newGameState)

  private def endAction(state: EngineState, action: GameAction): EngineState = 
    val gameState = state.gameState
    val isEndTurn = action == GameAction.EndTurn
    
    val (afterCardDraw, afterDecksManager, afterConquered) =
      if isEndTurn && state.territoryConqueredThisTurn then
        val currentPlayerId = gameState.turnManager.currentPlayer.id
        val (updatedGameState, updatedDecksManager) = drawTerritoryCard(gameState, gameState.decksManager, currentPlayerId)
        (updatedGameState, updatedDecksManager, false)
      else (gameState, gameState.decksManager, state.territoryConqueredThisTurn)
    
    val nextTurnManager = afterCardDraw.turnManager.nextPhase()
    val afterTurnManager = afterCardDraw.updateTurnManager(nextTurnManager).copy(decksManager = afterDecksManager)
    
    val newState = state.copy(
      gameState = afterTurnManager,
      strategicMoveUsed = if isEndTurn then false else state.strategicMoveUsed,
      territoryConqueredThisTurn = afterConquered
    )
    
    checkVictory(newState) match
      case Some(winner) => throw new GameOverException(winner)
      case None => newState

  def setGameState(newState: GameState): Unit =
    state = state.copy(gameState = newState)

  def getGameState: GameState = state.gameState

  def checkVictory: Option[PlayerState] =
    state.gameState.checkWinCondition

  private def checkVictory(state: EngineState): Option[PlayerState] =
    state.gameState.checkWinCondition