package engine

import model.player._
import model.cards._
import model.board._
import exceptions._
import utils._
import utils.GameEngineUtils._
import scala.util.Random

case class EngineState(
  gameState: GameState,
  pendingAttack: Option[(PlayerImpl, PlayerImpl, Territory, Territory, Int)] = None,
  territoryConqueredThisTurn: Boolean = false,
)

class GameEngine(
    val players: List[PlayerImpl],
    val gameId: String = java.util.UUID.randomUUID().toString,
    val botController: Option[BotController] = None
):
  private val (continents, _) = CardsBuilder.createBoard()
  private val board = Board(gameId, continents)
  private val territoryDeck = CardsBuilder.createTerritoriesDeck()
  private val objectiveDeck = CardsBuilder.createObjectivesDeck()
  private val playerStates = players.map(p => PlayerState(p, Set.empty, None, TurnPhase.WaitingForTurn, 0))
  private val turnManager = TurnManagerImpl(players)
  private val decksManager = DecksManagerImpl(territoryDeck, objectiveDeck)
  private var engineState = EngineState(
    GameState(
      gameId = gameId,
      board = board,
      playerStates = playerStates,
      turnManager = turnManager,
      decksManager = decksManager
    )
  )

  def setup(): GameState = 
    var currentDecksManager = decksManager.shuffleTerritoriesDeck().shuffleObjectivesDeck()
    val updatedBoard = distributeInitialTerritories()
    val (updatedDeckManager, playerStatesWithObjectives) = assignObjectivesToPlayers(currentDecksManager)
    currentDecksManager = updatedDeckManager
    val playerStatesWithTroops = BonusCalculator.calculateInitialTroops(players, playerStatesWithObjectives, updatedBoard)
    val firstPlayer = Random.shuffle(players).head
    val firstPlayerIndex = players.indexOf(firstPlayer)
    val updatedTurnManager = TurnManagerImpl(
      players = players,
      currentPlayerIndex = firstPlayerIndex,  
      phase = TurnPhase.SetupPlacing         
    )
    val updatedGameState = GameState(
      gameId = gameId,
      board = updatedBoard,
      playerStates = playerStatesWithTroops,
      turnManager = updatedTurnManager,
      decksManager = currentDecksManager
    )
    engineState = EngineState(
      gameState = updatedGameState,
      pendingAttack = None,
      territoryConqueredThisTurn = false,
    ) 
    updatedGameState

  def processAction(action: GameAction): GameState =
    try {
      val newState = performActions(engineState, action)
      engineState = newState
      newState.gameState
    } catch {
      case e: Exception => throw e
    }

  def performActions(engineState: EngineState, action: GameAction): EngineState =
    val gameState = engineState.gameState
    if !gameState.turnManager.isValidAction(action, gameState, engineState) then throw new InvalidActionException()
    action match
      case GameAction.EndSetup => endSetup(engineState)
      case GameAction.PlaceTroops(playerId, troops, territoryName) => placeTroopsAction(gameState, playerId, engineState, territoryName, troops)
      case GameAction.Reinforce(playerId, from, to, troops) => reinforceAction(from, gameState, playerId, engineState, to, troops)
      case GameAction.Attack(attackerId, defenderId, from, to, troops) => attackAction(attackerId, defenderId, from, gameState, engineState, to, troops)
      case GameAction.Defend(defenderId, territoryName, defendTroops) => defendAction(defendTroops, defenderId, gameState, engineState, territoryName)
      case GameAction.TradeCards(cards) => tradeCardsAction(cards, gameState, engineState)
      case GameAction.EndAttack | GameAction.EndPhase | GameAction.EndTurn => endAction(engineState, action)

  private def distributeInitialTerritories(): Board =
    val shuffledTerritories = Random.shuffle(board.territories)
    val assignedTerritories = shuffledTerritories.zipWithIndex.map: 
      case (territory, index) =>
        val playerIndex = index % players.size
        val player = players(playerIndex)
        territory.copy(owner = Some(player), troops = 1)
    assignedTerritories.foldLeft(board):
      (updatedBoard, territory) => updatedBoard.updatedTerritory(territory)
  
  private def assignObjectivesToPlayers(currentDecksManager: DecksManager): (DecksManager, List[PlayerState]) = 
    players.foldLeft((currentDecksManager, List.empty[PlayerState])) {
      case ((dm, states), player) =>
        val (updatedDM, objective) = dm.drawObjective()
        val playerState = playerStates.find(_.playerId == player.id).get
        (updatedDM, states :+ playerState.setObjectiveCard(objective))
    }

  private def placeTroopsAction(gameState: GameState, playerId: String, state: EngineState, territoryName: String, troops: Int): EngineState =
    val territory = gameState.getTerritoryByName(territoryName).get
    val playerState = gameState.getPlayerState(playerId).get
    val updatedTerritory = territory.addTroops(troops)
    val updatedPlayerState = playerState.copy(bonusTroops = playerState.bonusTroops - troops)
    val updatedGameState = gameState
      .updateBoard(gameState.board.updatedTerritory(updatedTerritory))
      .updatePlayerState(playerId, updatedPlayerState)
    state.copy(gameState = updatedGameState)

  private def reinforceAction(from: String, gameState: GameState, playerId: String, state: EngineState, to: String, troops: Int): EngineState =
    val fromTerritory = gameState.getTerritoryByName(from).get
    val toTerritory = gameState.getTerritoryByName(to).get
    val updatedFrom = fromTerritory.removeTroops(troops)
    val updatedTo = toTerritory.addTroops(troops)
    val updatedBoard = gameState.board
      .updatedTerritory(updatedFrom)
      .updatedTerritory(updatedTo)
    state.copy(gameState = gameState.updateBoard(updatedBoard))

  private def attackAction(attackerId: String, defenderId: String, from: String, gameState: GameState, state: EngineState, to: String, troops: Int): EngineState =
    val attacker = players.find(_.id == attackerId).get
    val defender = players.find(_.id == defenderId).get
    val attackerTerritory = gameState.getTerritoryByName(from).get
    val defenderTerritory = gameState.getTerritoryByName(to).get
    val updatedTurnManager = gameState.turnManager.asInstanceOf[TurnManagerImpl].copy(phase = TurnPhase.Defending)
    val updatedGameState = gameState.updateTurnManager(updatedTurnManager)
    state.copy(
      gameState = updatedGameState,
      pendingAttack = Some((attacker, defender, attackerTerritory, defenderTerritory, troops))
    )  

  private def defendAction(defendTroops: Int, defenderId: String, gameState: GameState, engineState: EngineState, territoryName: String): EngineState =
    engineState.pendingAttack match
      case Some((attacker, defender, attackerTerritory, defenderTerritory, attackingTroops)) =>
        val (result, updatedAttackerTerritory, updatedDefenderTerritory) = Battle.battle(
          attacker, defender, attackerTerritory, defenderTerritory, attackingTroops,
          attackerDiceRoll = utils.Dice.roll,
          defenderDiceRoll = _ => utils.Dice.roll(defendTroops)
        )
        val conquered = updatedDefenderTerritory.isOwnedBy(attacker.id)
        val updatedBoard = gameState.board
          .updatedTerritory(updatedAttackerTerritory)
          .updatedTerritory(updatedDefenderTerritory)
        val updatedGameState = gameState.updateBoard(updatedBoard)
        val afterElimination = if !updatedBoard.territoriesOwnedBy(defender.id).nonEmpty
          then transferCardsOnElimination(updatedGameState, defender.id, attacker.id)
          else updatedGameState
        engineState.copy(
          gameState = afterElimination,
          pendingAttack = None,  
          territoryConqueredThisTurn = engineState.territoryConqueredThisTurn || conquered
        )
      case None => throw new InvalidActionException()

  private def tradeCardsAction(cards: Set[TerritoryCard], gameState: GameState, state: EngineState): EngineState =
    val currentPlayerId = gameState.turnManager.currentPlayer.id
    val playerState = gameState.getPlayerState(currentPlayerId).get
    val bonus = BonusCalculator.calculateTradeBonus(cards)
    val updatedPlayerState = playerState
      .removeTerritoryCards(cards)
      .copy(bonusTroops = playerState.bonusTroops + bonus)  
    val updatedGameState = gameState.updatePlayerState(currentPlayerId, updatedPlayerState)
    state.copy(gameState = updatedGameState)

  private def endSetup(state: EngineState): EngineState =
    val gameState = state.gameState
    val currentPlayerId = gameState.turnManager.currentPlayer.id
    val currentPlayerState = gameState.getPlayerState(currentPlayerId).get
    
    // Decide the next turn manager state
    val nextTurnManager = gameState.turnManager.nextPlayer()

    // Update the game state with the new turn manager
    var updatedGameState = gameState.updateTurnManager(nextTurnManager)
    
    // If we're transitioning to normal play, calculate bonus troops for the next player
    if (nextTurnManager.currentPhase == TurnPhase.PlacingTroops) 
      val nextPlayerId = nextTurnManager.currentPlayer.id
      val nextPlayerState = updatedGameState.getPlayerState(nextPlayerId).get
      val bonus = BonusCalculator.calculateStartTurnBonus(nextPlayerId, updatedGameState.board)
      val updatedPlayerState = nextPlayerState.copy(bonusTroops = bonus)
      updatedGameState = updatedGameState.updatePlayerState(nextPlayerId, updatedPlayerState)
    
    state.copy(gameState = updatedGameState)

  private def endAction(state: EngineState, action: GameAction): EngineState = 
    val gameState = state.gameState
    val isEndTurn = action == GameAction.EndTurn
    val (afterCardDraw, afterConquered) =
      if (isEndTurn && state.territoryConqueredThisTurn)
        val currentPlayerId = gameState.turnManager.currentPlayer.id
        val (updatedGameState, _) = drawTerritoryCard(gameState, gameState.decksManager, currentPlayerId)
        (updatedGameState, false)
      else (gameState, state.territoryConqueredThisTurn)  
    
    val nextTurnManager = afterCardDraw.turnManager.nextPhase()
    var updatedGameState = afterCardDraw.updateTurnManager(nextTurnManager)
    
    // If we're ending turn, calculate bonus troops for the next player
    if (isEndTurn) 
      val nextPlayerId = nextTurnManager.currentPlayer.id
      val nextPlayerState = updatedGameState.getPlayerState(nextPlayerId).get
      val bonus = BonusCalculator.calculateStartTurnBonus(nextPlayerId, updatedGameState.board)
      val updatedPlayerState = nextPlayerState.copy(bonusTroops = bonus)
      updatedGameState = updatedGameState.updatePlayerState(nextPlayerId, updatedPlayerState)
    
    val newState = state.copy(
      gameState = updatedGameState,
      territoryConqueredThisTurn = afterConquered
    )
    checkVictory(newState) match
      case Some(winner) => throw new GameOverException(winner)
      case None => newState

  def setGameState(newState: GameState): Unit = engineState = engineState.copy(gameState = newState)

  def getGameState: GameState = engineState.gameState

  private def checkVictory(engineState: EngineState): Option[PlayerState] = engineState.gameState.checkWinCondition

  //to do: collegamento con client
  def executeBotTurn(): GameState =
    val currentPlayer = engineState.gameState.turnManager.currentPlayer
    if currentPlayer.playerType != PlayerType.Bot || botController.isEmpty then
      throw new InvalidPlayerException()  
    var isCurrentTurn = true
    while isCurrentTurn do
      try {
        val action = botController.get.nextAction(engineState.gameState, currentPlayer.id)
        engineState = performActions(engineState, action)
        action match
          case GameAction.EndTurn => isCurrentTurn = false
          case _ => 
            val updatedPlayer = engineState.gameState.turnManager.currentPlayer
            if updatedPlayer.id != currentPlayer.id then isCurrentTurn = false
      } catch {
        case e: Exception => 
          println(s"Bot error: ${e.getMessage}")
          isCurrentTurn = false
      }
    engineState.gameState