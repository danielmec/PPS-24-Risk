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
    val gameId: String = java.util.UUID.randomUUID().toString
):
  private val (continents, territoriesMap) = CardsBuilder.createBoard()
  private val board = Board(gameId, continents)
  private val territoryDeck = CardsBuilder.createTerritoriesDeck()
  private val objectiveDeck = CardsBuilder.createObjectivesDeck()
  private val playerStates: List[PlayerState] = players.map{ player => PlayerState(player, Set.empty, None, TurnPhase.WaitingForTurn, 0) } 
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

  def setup(): GameState = 
    var currentDecksManager = decksManager.shuffleTerritoriesDeck().shuffleObjectivesDeck()
    val updatedBoard = distributeInitialTerritories()
    val (updatedDeckManager, playerStatesWithObjectives) = assignObjectivesToPlayers(currentDecksManager)
    currentDecksManager = updatedDeckManager
    val playerStatesWithTroops = calculateInitialTroops(playerStatesWithObjectives, updatedBoard)
    val firstPlayer = Random.shuffle(players).head
    val firstPlayerIndex = players.indexOf(firstPlayer)
    val updatedTurnManager = TurnManagerImpl(
      players = players,
      currentPlayerIndex = firstPlayerIndex,  
      phase = TurnPhase.PlacingTroops         
    )
    val updatedGameState = GameState(
      gameId = gameId,
      board = updatedBoard,
      playerStates = playerStatesWithTroops,
      turnManager = updatedTurnManager,
      decksManager = currentDecksManager
    )
    state = EngineState(
      gameState = updatedGameState,
      pendingAttack = None,
      territoryConqueredThisTurn = false,
    ) 
    updatedGameState

  def processAction(action: GameAction): GameState =
    try {
      val newState = performActions(state, action)
      state = newState
      newState.gameState
    } catch {
      case e: Exception => throw e
    }

  def performActions(state: EngineState, action: GameAction): EngineState =
    val gameState = state.gameState
    val gameStateWithBonus = 
      if (gameState.turnManager.currentPhase == TurnPhase.PlacingTroops) 
        val currentPlayerId = gameState.turnManager.currentPlayer.id
        val playerState = gameState.playerStates.find(_.playerId == currentPlayerId).get
        if (playerState.bonusTroops == 0) 
          val bonus = BonusCalculator.calculateStartTurnBonus(currentPlayerId, gameState.board)
          val updatedPlayerStates = gameState.playerStates.map:
            case playerState if (playerState.playerId == currentPlayerId) => playerState.copy(bonusTroops = playerState.bonusTroops + bonus)
            case playerState => playerState
          gameState.copy(playerStates = updatedPlayerStates)
        else gameState
      else gameState 
    val updatedState = state.copy(gameState = gameStateWithBonus)
    action match
      case GameAction.PlaceTroops(playerId, troops, territoryName) => placeTroopsAction(gameStateWithBonus, playerId, updatedState, territoryName, troops)
      case GameAction.Reinforce(playerId, from, to, troops) => reinforceAction(from, gameState, playerId, state, to, troops)
      case GameAction.Attack(attackerId, defenderId, from, to, troops) => attackAction(attackerId, defenderId, from, gameState, state, to, troops)
      case GameAction.Defend(defenderId, territoryName, defendTroops) => defendAction(defendTroops, defenderId, gameState, state, territoryName)
      case GameAction.TradeCards(cards) => tradeCardsAction(cards, gameState, state)
      case GameAction.EndAttack | GameAction.EndPhase | GameAction.EndTurn => endAction(state, action)

  private def distributeInitialTerritories(): Board =
    val shuffledTerritories = Random.shuffle(board.territories)
    val assignedTerritories = shuffledTerritories.zipWithIndex.map: 
      case (territory, index) =>
        val playerIndex = index % players.size
        val player = players(playerIndex)
        territory.copy(owner = Some(player), troops = 1)
    val updatedContinents = board.continents.map:
      continent =>
        val updatedTerritories = continent.territories.map:
          territory =>
            assignedTerritories.find(_.name == territory.name).getOrElse(territory)
        continent.copy(territories = updatedTerritories)
    board.copy(continents = updatedContinents)
  
  private def assignObjectivesToPlayers(currentDecksManager: DecksManager): (DecksManager, List[PlayerState]) = 
    players.foldLeft((currentDecksManager, List.empty[PlayerState])):
      case ((decksManager, updatedStates), player) =>
        val (updatedDecksMgr, objective) = decksManager.drawObjective()
        val playerState = playerStates.find(_.playerId == player.id).get
        val updatedPlayerState = playerState.setObjectiveCard(objective)
        (updatedDecksMgr, updatedStates :+ updatedPlayerState)

  private def calculateInitialTroops(playerStates: List[PlayerState], board: Board): List[PlayerState] = 
    val baseTroops = players.size match
      case 2 => 40
      case 3 => 35
      case 4 => 30
      case 5 => 25
      case _ => 20
    playerStates.map:
      playerState =>
        val playerId = playerState.playerId
        val gameStateWithBoard = state.gameState.copy(board = board)
        val territoriesOwned = countPlayerTerritories(gameStateWithBoard, playerId)
        val remainingTroops = baseTroops - territoriesOwned
        playerState.copy(bonusTroops = remainingTroops, phase = TurnPhase.PlacingTroops)

  private def placeTroopsAction(gameState: GameState, playerId: String, state: EngineState, territoryName: String, troops: Int): EngineState =
    val territory = gameState.board.territories.find(_.name == territoryName).getOrElse(throw new InvalidTerritoryException())
    val playerState = gameState.playerStates.find(_.playerId == playerId).getOrElse(throw new InvalidPlayerException())
    if !territory.isOwnedBy(playerId) then throw new InvalidTerritoryException()
    if troops <= 0 || troops > playerState.bonusTroops then throw new InvalidTroopsException()
    val updatedTerritory = territory.addTroops(troops)
    val updatedBoard = gameState.board.updatedTerritory(updatedTerritory)
    val updatedPlayerState = playerState.copy(bonusTroops = playerState.bonusTroops - troops)
    val updatedPlayerStates = gameState.playerStates.map:
      case playerState if playerState.playerId == playerId => updatedPlayerState
      case playerState => playerState      
    val newGameState = gameState.copy(board = updatedBoard, playerStates = updatedPlayerStates)
    state.copy(gameState = newGameState)

  private def reinforceAction(from: String, gameState: GameState, playerId: String, state: EngineState, to: String, troops: Int): EngineState =
    val fromTerritory = gameState.board.territories.find(_.name == from).getOrElse(throw new InvalidTerritoryException())
    val toTerritory = gameState.board.territories.find(_.name == to).getOrElse(throw new InvalidTerritoryException())
    if (!fromTerritory.isOwnedBy(playerId) || !toTerritory.isOwnedBy(playerId)) then throw new InvalidTerritoryException()
    if (!fromTerritory.hasEnoughTroops(troops + 1) || troops <= 0) then throw new InvalidTroopsException()
    if (!gameState.board.areNeighbors(fromTerritory, toTerritory)) then throw new InvalidTerritoryException()
    val updatedFrom = fromTerritory.removeTroops(troops)
    val updatedTo = toTerritory.addTroops(troops)
    val updatedBoard = gameState.board.updatedTerritory(updatedFrom).updatedTerritory(updatedTo)
    val newGameState = gameState.copy(board = updatedBoard)
    state.copy(gameState = newGameState)

  private def attackAction(attackerId: String, defenderId: String, from: String, gameState: GameState, state: EngineState, to: String, troops: Int): EngineState =
    val attacker = players.find(_.id == attackerId).getOrElse(throw new InvalidPlayerException())
    val defender = players.find(_.id == defenderId).getOrElse(throw new InvalidPlayerException()) 
    val attackerTerritory = gameState.board.territories.find(_.name == from).getOrElse(throw new InvalidTerritoryException())
    val defenderTerritory = gameState.board.territories.find(_.name == to).getOrElse(throw new InvalidTerritoryException())
    if !attackerTerritory.hasEnoughTroops(troops + 1) || troops <= 0 then throw new InvalidTroopsException()
    if !attackerTerritory.isOwnedBy(attackerId) || !defenderTerritory.isOwnedBy(defenderId) then throw new InvalidTerritoryException()
    if (!gameState.board.areNeighbors(attackerTerritory, defenderTerritory)) then throw new InvalidTerritoryException()
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
          val updatedBoard = gameState.board
            .updatedTerritory(updatedAttackerTerritory)
            .updatedTerritory(updatedDefenderTerritory)          
          val updatedGameState = gameState.updateBoard(updatedBoard)  
          val afterElimination =
            if !updatedBoard.territories.exists(_.owner.exists(_.id == defender.id))
            then transferCardsOnElimination(updatedGameState, defender.id, attacker.id)
            else updatedGameState
          state.copy(
            gameState = afterElimination,
            pendingAttack = None,
            territoryConqueredThisTurn = state.territoryConqueredThisTurn || conquered
          )        
      case _ => throw new InvalidActionException()

  private def tradeCardsAction(cards: Set[TerritoryCard], gameState: GameState, state: EngineState): EngineState =
    val currentPlayerId = gameState.turnManager.currentPlayer.id
    val playerState = gameState.getPlayerState(currentPlayerId).getOrElse(throw new InvalidPlayerException())
    if cards.size != 3 then throw new InvalidCardException()
    if !cards.subsetOf(playerState.territoryCards) then throw new InvalidCardException()     
    val bonus = BonusCalculator.calculateTradeBonus(cards)     
    if bonus == 0 then throw new InvalidCardException()   
    val updatedPlayerState = playerState
      .removeTerritoryCards(cards)
      .copy(bonusTroops = playerState.bonusTroops + bonus)  
    val updatedGameState = gameState.updatePlayerState(currentPlayerId, updatedPlayerState)
    state.copy(gameState = updatedGameState)

  private def endAction(state: EngineState, action: GameAction): EngineState = 
    val gameState = state.gameState
    val isEndTurn = action == GameAction.EndTurn
    val (afterCardDraw, afterConquered) =
      if isEndTurn && state.territoryConqueredThisTurn then
        val currentPlayerId = gameState.turnManager.currentPlayer.id
        val (updatedGameState, _) = drawTerritoryCard(gameState, gameState.decksManager, currentPlayerId)
        (updatedGameState, false)
      else (gameState, state.territoryConqueredThisTurn)
    val nextTurnManager = afterCardDraw.turnManager.nextPhase()
    val updatedGameState = afterCardDraw.updateTurnManager(nextTurnManager)
    val newState = state.copy(
      gameState = updatedGameState,
      territoryConqueredThisTurn = afterConquered
    ) 
    checkVictory(newState) match
      case Some(winner) => throw new GameOverException(winner)
      case None => newState

  def setGameState(newState: GameState): Unit = state = state.copy(gameState = newState)

  def getGameState: GameState = state.gameState

  def checkVictory: Option[PlayerState] = state.gameState.checkWinCondition

  private def checkVictory(state: EngineState): Option[PlayerState] = state.gameState.checkWinCondition