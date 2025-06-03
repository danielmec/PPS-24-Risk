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
    territoryCards: List[TerritoryCard] = List.empty,
    objectiveCards: List[ObjectiveCard] = List.empty
):

    def getPlayerState(playerId: String): Option[PlayerState] =
        playerStates.find(_.playerId == turnManager.currentPlayer.id)

    def updatePlayerState(playerId: String, newState: PlayerState): GameState = 
        copy(playerStates = playerStates.map:
            case player if player.playerId == playerId => newState
            case player => player
        )

    def updateDecksManager(newDecksManager: DecksManager): GameState = copy(decksManager = newDecksManager)

    def updateBoard(newBoard: Board): GameState = copy(board = newBoard)

    def updateTurnManager(newTurnManager: TurnManager): GameState = copy(turnManager = newTurnManager)
 
    def checkWinCondition: Option[PlayerState] =
        playerStates.find(player => player.objectiveCard.exists(
            obj => ObjectiveValidator.done(obj, this, player)
            )).map(_.playerId).flatMap(
                id => playerStates.find(_.playerId == id))
