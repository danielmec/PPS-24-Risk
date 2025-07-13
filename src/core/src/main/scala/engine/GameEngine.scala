package engine

import model.player._
import model.cards._
import model.board._
import exceptions._
import utils._
import utils.GameEngineUtils._
import scala.util.Random._

case class EngineState(
  gameState: GameState,
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
  private val playerStates = players.map(p => PlayerState(p, Set.empty, None, TurnPhase.SetupPhase, 0))
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


  /**
    * Initializes the game state by shuffling the decks, distributing territories, and assigning objectives.
    * @return the updated GameState after setup
    */
  def setup(): GameState = 
    var currentDecksManager = decksManager.shuffleTerritoriesDeck().shuffleObjectivesDeck()
    val updatedBoard = distributeInitialTerritories(board, players)
    val (updatedDeckManager, playerStatesWithObjectives) = assignObjectivesToPlayers(currentDecksManager, players, playerStates)
    currentDecksManager = updatedDeckManager
    val shuffledPlayers = shuffle(players)
    val playerStatesWithTroops = BonusCalculator.calculateInitialTroops(shuffledPlayers, playerStatesWithObjectives, updatedBoard)
    val updatedTurnManager = TurnManagerImpl(
      players = shuffledPlayers,
      phase = TurnPhase.SetupPhase  
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
      territoryConqueredThisTurn = false,
    ) 
    updatedGameState

  /**
   * Processes a game action and updates the game state accordingly.
   * @param action the action to process
   * @return the updated GameState after processing the action
   */
  def processAction(action: GameAction): GameState =
    try {
      val newState = performActions(engineState, action)
      engineState = newState
      newState.gameState
    } catch {
      case e: Exception => throw e
    }

  /**
    * Performs the actions based on the game action provided.
    * Validates the action and updates the game state accordingly.
    * @param engineState the current state of the engine
    * @param action the game action to perform
    * @return the updated EngineState after performing the action
    */
  def performActions(engineState: EngineState, action: GameAction): EngineState =
    val gameState = engineState.gameState
    if !gameState.turnManager.isValidAction(action, gameState, engineState) then throw new InvalidActionException()
    action match
      case GameAction.EndSetup => endSetup(engineState)
      case GameAction.PlaceTroops(playerId, troops, territoryName) => placeTroopsAction(gameState, playerId, engineState, territoryName, troops)
      case GameAction.Reinforce(playerId, from, to, troops) => reinforceAction(from, gameState, playerId, engineState, to, troops)
      case GameAction.Attack(attackerId, defenderId, from, to, troops) => attackAction(attackerId, defenderId, from, gameState, engineState, to, troops)
      case GameAction.TradeCards(playerId, cardNames) => tradeCardsAction(playerId, cardNames, gameState, engineState)
      case GameAction.EndTurn => endAction(engineState, action)

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

  private def attackAction(
    attackerId: String,
    defenderId: String,
    from: String,
    gameState: GameState,
    state: EngineState,
    to: String,
    troops: Int
  ): EngineState = {
    val attackerTerritoryOpt = gameState.getTerritoryByName(from).flatMap(t => t.owner.map(owner => (owner, t)))
    val defenderTerritoryOpt = gameState.getTerritoryByName(to).flatMap(t => t.owner.map(owner => (owner, t)))

    (attackerTerritoryOpt, defenderTerritoryOpt) match {
      case (Some((attacker, attackerTerritory)), Some((defender, defenderTerritory))) =>
        Battle.battleRound(attacker, defender, attackerTerritory, defenderTerritory, troops) match
          case Left(error) =>
            throw new IllegalStateException(s"Battle failed: $error")
          case Right(battleResult) =>
            val updatedBoard = gameState.board
              .updatedTerritory(battleResult.attackerTerritory)
              .updatedTerritory(battleResult.defenderTerritory)
            val updatedGameState = gameState
              .updateBoard(updatedBoard)
              .updateLastBattleResult(battleResult)
            val conquered = battleResult.result == BattleResult.AttackerWins
            state.copy(
              gameState = updatedGameState,
              territoryConqueredThisTurn = state.territoryConqueredThisTurn || conquered
            )
      case _ =>
        throw new IllegalStateException("Invalid attacker or defender territory/owner")
    }
  }

  private def tradeCardsAction(playerId: String, cardNames: Set[String], gameState: GameState, state: EngineState): EngineState =
    val playerState = gameState.getPlayerState(playerId).get
    val cardNameCounts = cardNames.groupBy(identity).view.mapValues(_.size).toMap
    val cards: Seq[TerritoryCard] = playerState.territoryCards
      .groupBy(_.territory.name)
      .flatMap { case (name, cards) =>
        val count = cardNameCounts.getOrElse(name, 0)
        cards.take(count)
      }.toSeq
    val bonus = BonusCalculator.calculateTradeBonus(cards)
    val updatedPlayerState = playerState
      .removeTerritoryCards(cards.toSet)
      .copy(bonusTroops = playerState.bonusTroops + bonus)
    val updatedGameState = gameState.updatePlayerState(playerId, updatedPlayerState)
    state.copy(gameState = updatedGameState)

  private def endSetup(state: EngineState): EngineState =
    val gameState = state.gameState
    val currentPlayerId = gameState.turnManager.currentPlayer.id
    val currentPlayerState = gameState.getPlayerState(currentPlayerId).get
    val nextTurnManager = gameState.turnManager.nextPlayer()
    var updatedGameState = gameState.updateTurnManager(nextTurnManager)
    if (nextTurnManager.currentPhase == TurnPhase.MainPhase) 
      val nextPlayerId = nextTurnManager.currentPlayer.id
      val nextPlayerState = updatedGameState.getPlayerState(nextPlayerId).get
      val bonus = BonusCalculator.calculateStartTurnBonus(nextPlayerId, updatedGameState.board)
      val updatedPlayerState = nextPlayerState.copy(bonusTroops = bonus)
      updatedGameState = updatedGameState.updatePlayerState(nextPlayerId, updatedPlayerState)
    state.copy(gameState = updatedGameState)

  private def endAction(state: EngineState, action: GameAction): EngineState = 
    val stateAfterCardDraw = handleTerritoryCardReward(state, action)
    val stateWithNextPlayer = moveToNextPlayer(stateAfterCardDraw)
    checkVictoryCondition(stateWithNextPlayer)

  private var battleResultCallback: Option[(String, String, String, String, BattleRoundResult) => Unit] = None
  
  /**
    * Sets a callback to handle battle results during bot turns.
    * This callback will be invoked with the following parameters:
    * - from: The name of the attacking territory
    * - to: The name of the defending territory
    * - attackerId: The ID of the attacking player
    * - defenderId: The ID of the defending player
    * - battleResult: The result of the battle round
    * @param callback
    */
  def setBattleResultCallback(callback: (String, String, String, String, BattleRoundResult) => Unit): Unit =
    battleResultCallback = Some(callback)

  /**
    * Executes the bot's turn based on the current game state.
    * It checks if the current player is a bot and executes the turn accordingly.
    * @param engineState the current state of the game engine
    * @return the updated GameState after executing the bot's turn
    * @throws InvalidPlayerException if the current player is not a bot or if the bot controller is not set
    */
  def executeBotTurn(): GameState =
    val currentPlayer = engineState.gameState.turnManager.currentPlayer
    if currentPlayer.playerType != PlayerType.Bot || botController.isEmpty then
      throw new InvalidPlayerException()  
    println(s"=== ESECUZIONE TURNO BOT ===")
    println(s"Bot: ${currentPlayer.id}")
    println(s"Fase: ${engineState.gameState.turnManager.currentPhase}")
    engineState.gameState.turnManager.currentPhase match {
      case TurnPhase.SetupPhase => executeSetupTurn()
      case TurnPhase.MainPhase => executeMainTurn()
    }
    println(s"=============================")
    engineState.gameState

  private def executeSetupTurn(): Unit =
    val currentPlayer = engineState.gameState.turnManager.currentPlayer
    var currentPlayerState = engineState.gameState.getPlayerState(currentPlayer.id).get
    val botTerritories = engineState.gameState.board.territories.filter(t => t.owner.exists(_.id == currentPlayer.id))
    if botTerritories.isEmpty then return
    
    while currentPlayerState.bonusTroops > 0 do      
      try {
        val action = botController.get.nextAction(engineState.gameState, currentPlayer.id)        
        engineState = performActions(engineState, action)
        currentPlayerState = engineState.gameState.getPlayerState(currentPlayer.id).get        
      } catch {
        case e: Exception =>
          println(s"ERRORE DURANTE SETUP: ${e.getMessage}")
          println(s"Tipo eccezione: ${e.getClass.getSimpleName}")
          e.printStackTrace()
          try {
            engineState = performActions(engineState, GameAction.EndSetup)
          } catch {
            case _: Exception => 
          }
          return
      }
    try {
      engineState = performActions(engineState, GameAction.EndSetup)
    } catch {
      case e: Exception =>
        println(s"Errore nel terminare setup: ${e.getMessage}")
    }

  private def executeMainTurn(): Unit =
    val currentPlayer = engineState.gameState.turnManager.currentPlayer
    val playerState = engineState.gameState.getPlayerState(currentPlayer.id).get
    if playerState.bonusTroops > 0 then placeAllBonusTroops()
    executeAllAttacks() 
    executeReinforceOrEnd()

  private def placeAllBonusTroops(): Unit =
    var currentPlayerState = engineState.gameState.getPlayerState(engineState.gameState.turnManager.currentPlayer.id).get
    while currentPlayerState.bonusTroops > 0 do
      val action = botController.get.nextAction(engineState.gameState, engineState.gameState.turnManager.currentPlayer.id)
      engineState = performActions(engineState, action)
      currentPlayerState = engineState.gameState.getPlayerState(engineState.gameState.turnManager.currentPlayer.id).get

  private def executeAllAttacks(): Unit =
    var attackCount = 0
    val maxAttacks = 10
    var canContinueAttacking = true
    while canContinueAttacking && attackCount < maxAttacks do      
      try {
        val action = botController.get.nextAction(engineState.gameState, engineState.gameState.turnManager.currentPlayer.id)
        action match
          case attack: GameAction.Attack =>
            engineState = performActions(engineState, action)
            val updatedState = engineState.gameState
            updatedState.lastBattleResult match
              case Some(battleResult) =>  
                battleResultCallback.foreach { callback =>
                  callback(attack.from, attack.to, attack.attackerId, attack.defenderId, battleResult)
                }                          
              case None => canContinueAttacking = false
        
          case _ => canContinueAttacking = false
            
      } catch {
        case e: Exception =>
          println(s"Attacco ${attackCount + 1} fallito: ${e.getMessage}")
          canContinueAttacking = false
    }
    
  private def executeOneAttack(): Unit = executeAllAttacks()


  private def executeReinforceOrEnd(): Unit =
    try {
      val action = botController.get.nextAction(engineState.gameState, engineState.gameState.turnManager.currentPlayer.id)
      action match
        case _: GameAction.Reinforce =>
          engineState = performActions(engineState, action)
          engineState = performActions(engineState, GameAction.EndTurn)
        case GameAction.EndTurn =>
          engineState = performActions(engineState, action)
        case _ =>
          engineState = performActions(engineState, GameAction.EndTurn)
    } catch {
      case e: Exception =>
        println(s"Errore nel rinforzo: ${e.getMessage}")
        try {
          engineState = performActions(engineState, GameAction.EndTurn)
        } catch {
          case _: Exception => 
        }
    }

  def setGameState(newState: GameState): Unit = engineState = engineState.copy(gameState = newState)

  def getGameState: GameState = engineState.gameState