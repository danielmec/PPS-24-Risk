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

  def executeBotTurn(): GameState =
    val currentPlayer = engineState.gameState.turnManager.currentPlayer
    if currentPlayer.playerType != PlayerType.Bot || botController.isEmpty then
      throw new InvalidPlayerException()
      
    println(s"=== ESECUZIONE TURNO BOT ===")
    println(s"Bot: ${currentPlayer.id}")
    println(s"Fase: ${engineState.gameState.turnManager.currentPhase}")
    
    var isCurrentTurn = true
    var actionCount = 0
    val maxActions = engineState.gameState.turnManager.currentPhase match {
      case TurnPhase.SetupPhase => 50  // ← Aumentato per SetupPhase
      case TurnPhase.MainPhase => 10   // ← Limite normale per MainPhase
    }
    
    while isCurrentTurn && actionCount < maxActions do
      try {
        actionCount += 1
        
        val playerState = engineState.gameState.getPlayerState(currentPlayer.id).get
        
        // ← GESTIONE SETUP PHASE
        if engineState.gameState.turnManager.currentPhase == TurnPhase.SetupPhase && playerState.bonusTroops == 0 then
          println(s"Bot ha finito le truppe bonus (${playerState.bonusTroops}), termino il turno")
          engineState = performActions(engineState, GameAction.EndSetup)
          isCurrentTurn = false
        // ← GESTIONE MAIN PHASE - FORZA PLACE TROOPS SE CI SONO BONUS
        else if engineState.gameState.turnManager.currentPhase == TurnPhase.MainPhase && playerState.bonusTroops > 0 then
          println(s"Richiesta azione #$actionCount (truppe rimanenti: ${playerState.bonusTroops}) - DEVE piazzare truppe")
          
          // Forza il piazzamento delle truppe bonus
          val botTerritories = engineState.gameState.board.territories.filter(_.owner.exists(_.id == currentPlayer.id))
          if botTerritories.nonEmpty then
            val targetTerritory = botTerritories.head // Scegli il primo territorio disponibile
            val placeTroopsAction = GameAction.PlaceTroops(currentPlayer.id, 1, targetTerritory.name)
            println(s"Bot forza piazzamento su: ${targetTerritory.name}")
            
            engineState = performActions(engineState, placeTroopsAction)
            println(s"Azione eseguita con successo")
          else
            println("ERRORE: Bot non ha territori disponibili!")
            isCurrentTurn = false
        else
          println(s"Richiesta azione #$actionCount (truppe rimanenti: ${playerState.bonusTroops})")
          
          val action = botController.get.nextAction(engineState.gameState, currentPlayer.id)
          val actionName = action match
            case GameAction.EndTurn => "EndTurn"
            case GameAction.EndSetup => "EndSetup"
            case GameAction.PlaceTroops(_, troops, territory) => s"PlaceTroops($troops su $territory)"
            case GameAction.Attack(_, _, from, to, troops) => s"Attack($from -> $to con $troops)"
            case GameAction.Reinforce(_, from, to, troops) => s"Reinforce($from -> $to con $troops)"
            case GameAction.TradeCards(cards, _) => s"TradeCards(${cards.size} carte)"
            case _ => action.getClass.getSimpleName

          println(s"Bot ha scelto: $actionName")
          
          engineState = performActions(engineState, action)
          println(s"Azione eseguita con successo")
          
          action match
            case GameAction.EndTurn => 
              println("Bot ha terminato il turno")
              isCurrentTurn = false
            case GameAction.EndSetup =>
              println("Bot ha terminato il setup")
              isCurrentTurn = false
            case _ => 
              val updatedPlayer = engineState.gameState.turnManager.currentPlayer
              if updatedPlayer.id != currentPlayer.id then 
                println(s"Turno passato a: ${updatedPlayer.id}")
                isCurrentTurn = false
      } catch {
        case e: Exception => 
          println(s"Bot error: ${e.getMessage}")
          e.printStackTrace()
          isCurrentTurn = false
      }
    
    if (actionCount >= maxActions) {
      println(s"ATTENZIONE: Bot ha raggiunto il limite di azioni ($maxActions), termino forzatamente")
      if engineState.gameState.turnManager.currentPhase == TurnPhase.SetupPhase then
        try {
          engineState = performActions(engineState, GameAction.EndSetup)
        } catch {
          case _: Exception => // Ignora errori di validazione
        }
    }
    
    println(s"=============================")
    engineState.gameState

  def setGameState(newState: GameState): Unit = engineState = engineState.copy(gameState = newState)

  def getGameState: GameState = engineState.gameState