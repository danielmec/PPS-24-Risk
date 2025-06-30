package engine
import model.cards.* 
import model.player.*
import model.board.*

case class GameState(
    gameId: String,
    board: Board,
    playerStates: List[PlayerState],
    turnManager: TurnManager,
    decksManager: DecksManager,
    objectiveCards: List[ObjectiveCard] = List.empty,
    lastBattleResult: Option[BattleRoundResult] = None, // Aggiunto campo per memorizzare l'ultimo risultato di battaglia
    playerStartedTurn: Boolean = true  // Aggiunto per tracciare lo stato di inizio turno
):
  def territories: List[Territory] = board.territories
  
  def getPlayerState(playerId: String): Option[PlayerState] =
    playerStates.find(_.playerId == playerId)
  
  def updatePlayerState(playerId: String, newState: PlayerState): GameState =
    copy(playerStates = playerStates.map {
      case ps if ps.playerId == playerId => newState
      case ps => ps
    })
    
  def updateBoard(newBoard: Board): GameState =
    copy(board = newBoard)
    
  def updateTurnManager(newTurnManager: TurnManager): GameState =
    copy(turnManager = newTurnManager)
    
  def updateDecksManager(newDecksManager: DecksManager): GameState =
    copy(decksManager = newDecksManager)
  
  def updateLastBattleResult(battleResult: BattleRoundResult): GameState =
    copy(lastBattleResult = Some(battleResult))
    
  def getTerritoryByName(name: String): Option[Territory] =
    board.getTerritoryByName(name)
    
  def checkWinCondition: Option[PlayerState] =
    playerStates.find(playerState => 
      playerState.objectiveCard.exists(objective => 
        ObjectiveValidator.done(objective, this, playerState)
      )
    )
