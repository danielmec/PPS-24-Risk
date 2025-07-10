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
  ): EngineState =
    val attackerTerritory = gameState.getTerritoryByName(from).get
    val defenderTerritory = gameState.getTerritoryByName(to).get
    Battle.battleRound(attackerTerritory.owner.get, defenderTerritory.owner.get, attackerTerritory, defenderTerritory, troops) match
      case Left(error) =>
        throw new IllegalStateException(s"Battle failed: $error")
      case Right(battleResult) =>
        val updatedBoard = gameState.board
          .updatedTerritory(battleResult.attackerTerritory)
          .updatedTerritory(battleResult.defenderTerritory)
        var updatedGameState = gameState
          .updateBoard(updatedBoard)
          .updateLastBattleResult(battleResult)
        val conquered = battleResult.result == BattleResult.AttackerWins
        if conquered && !updatedBoard.territoriesOwnedBy(defenderId).nonEmpty then
          updatedGameState = transferCardsOnElimination(updatedGameState, defenderId, attackerId)
        state.copy(
          gameState = updatedGameState,
          territoryConqueredThisTurn = state.territoryConqueredThisTurn || conquered
        )

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

  // Callback per notificare battle results durante il turno bot
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
   * Handles bot actions during their turn.
   * If the current player is a bot, it executes the bot's turn logic.   
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

  /**
    * Executes the setup turn for the bot.
    * If the bot has no bonus troops left, it ends the setup phase.
    * Otherwise, it places troops according to the bot's strategy. 
   */
  private def executeSetupTurn(): Unit =
    val currentPlayer = engineState.gameState.turnManager.currentPlayer
    var currentPlayerState = engineState.gameState.getPlayerState(currentPlayer.id).get
    
    println(s"=== DEBUG SETUP TURN ===")
    println(s"Current Bot: ${currentPlayer.id} (${currentPlayer.name})")
    println(s"Bot Controller: ${botController.map(_.getClass.getSimpleName).getOrElse("NONE")}")
    println(s"Bonus Troops: ${currentPlayerState.bonusTroops}")
    
    val botTerritories = engineState.gameState.board.territories.filter(t => t.owner.exists(_.id == currentPlayer.id))
    println(s"Bot Territories: ${botTerritories.size}")
    botTerritories.foreach(t => println(s"  - ${t.name}: ${t.troops} troops"))
    
    if botTerritories.isEmpty then
      println(s"ERRORE CRITICO: Bot ${currentPlayer.id} non ha territori!")
      return
    
    while currentPlayerState.bonusTroops > 0 do
      println(s"Bot piazza truppe in setup (${currentPlayerState.bonusTroops} rimaste)")
      
      try {
        println(s"Chiamando nextAction per bot ${currentPlayer.id}...")
        val action = botController.get.nextAction(engineState.gameState, currentPlayer.id)
        println(s"Azione ricevuta: $action")
        
        engineState = performActions(engineState, action)
        currentPlayerState = engineState.gameState.getPlayerState(currentPlayer.id).get
        
        println(s"Azione eseguita con successo. Truppe rimaste: ${currentPlayerState.bonusTroops}")
        
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
    
    println("Bot termina il setup - nessuna truppa rimanente")
    try {
      engineState = performActions(engineState, GameAction.EndSetup)
    } catch {
      case e: Exception =>
        println(s"Errore nel terminare setup: ${e.getMessage}")
    }

  /**
  * Executes the main turn for the bot.
  * It performs the following steps:
  * 1. Place all bonus troops.
  * 2. Execute all possible attacks consecutively.
  * 3. Reinforce or end the turn.
  */
  private def executeMainTurn(): Unit =
    val currentPlayer = engineState.gameState.turnManager.currentPlayer
    val playerState = engineState.gameState.getPlayerState(currentPlayer.id).get
    
    if playerState.bonusTroops > 0 then
      placeAllBonusTroops()
    
    executeAllAttacks()
    
    executeReinforceOrEnd()


  /**
    * Places all bonus troops for the current player.
    * It continues to place troops until there are no bonus troops left.
    * The bot will call its controller to get the next action for placing troops.
   */
  private def placeAllBonusTroops(): Unit =
    var currentPlayerState = engineState.gameState.getPlayerState(engineState.gameState.turnManager.currentPlayer.id).get
    
    while currentPlayerState.bonusTroops > 0 do
      println(s"Piazzo truppe: ${currentPlayerState.bonusTroops} rimaste")
      val action = botController.get.nextAction(engineState.gameState, engineState.gameState.turnManager.currentPlayer.id)
      engineState = performActions(engineState, action)
      currentPlayerState = engineState.gameState.getPlayerState(engineState.gameState.turnManager.currentPlayer.id).get
    
    println("Tutte le truppe bonus piazzate")

  /**
    * Executes all possible attacks for the current player consecutively.
    * The bot will continue attacking until no more valid attacks are available.
    * Each attack is notified to human players via callback without interrupting the flow.
    * A 3-second delay is added between attacks to allow clients to process battle results.
  */
  private def executeAllAttacks(): Unit =
    println("=== FASE ATTACCHI MULTIPLI ===")
    var attackCount = 0
    val maxAttacks = 10
    var canContinueAttacking = true
    
    while canContinueAttacking && attackCount < maxAttacks do
      println(s"Tentativo di attacco ${attackCount + 1}...")
      
      try {
        // action viene ottenuta dal bot controller che a sua volta lo ottiene dal prologRule che lo converte in un'azione
        val action = botController.get.nextAction(engineState.gameState, engineState.gameState.turnManager.currentPlayer.id)
        action match
          case attack: GameAction.Attack =>
            println(s"Bot esegue attacco ${attackCount + 1}: $attack")
            engineState = performActions(engineState, action)
            
            val updatedState = engineState.gameState
            updatedState.lastBattleResult match
              case Some(battleResult) =>  
                battleResultCallback.foreach { callback =>
                  callback(attack.from, attack.to, attack.attackerId, attack.defenderId, battleResult)
                }
                attackCount += 1
                println(s"Attacco ${attackCount} completato con successo")
                          
              case None =>
                println("ATTENZIONE: Nessun battle result trovato dopo l'attacco del bot")
                canContinueAttacking = false
        
          case _ =>
            println(s"Bot non ha più attacchi validi dopo ${attackCount} attacchi")
            canContinueAttacking = false
            
      } catch {
        case e: Exception =>
          println(s"Attacco ${attackCount + 1} fallito: ${e.getMessage}")
          canContinueAttacking = false
    }
    
    println(s"=== FINE ATTACCHI: ${attackCount} attacchi eseguiti ===")

  /**
    * Legacy method - kept for compatibility but now calls executeAllAttacks
  */
  private def executeOneAttack(): Unit = executeAllAttacks()


  /**
    * Reinforces or ends the turn for the current player.
    * If the bot decides to reinforce, it will call its controller to get the next action.
    * If the bot chooses to end the turn, it will perform the end turn action.
    * If an error occurs during reinforcement, it will attempt to end the turn instead. 
   */
  private def executeReinforceOrEnd(): Unit =
    println("Rinforzo o fine turno...")
    try {
      val action = botController.get.nextAction(engineState.gameState, engineState.gameState.turnManager.currentPlayer.id)
      action match
        case _: GameAction.Reinforce =>
          println(s"Bot rinforza: $action")
          engineState = performActions(engineState, action)
          engineState = performActions(engineState, GameAction.EndTurn)
        case GameAction.EndTurn =>
          println("Bot termina il turno")
          engineState = performActions(engineState, action)
        case _ =>
          println("Bot sceglie azione non valida, termino il turno")
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