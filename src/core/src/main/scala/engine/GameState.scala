package engine
import model.cards.* 
import model.player.*
import model.board.*

case class GameState(
    gameId: String,
    board: Board,
    states: List[PlayerState],
    turnManager: TurnManager,
    decksManager: DecksManager,
    territoryCards: List[TerritoryCard] = List.empty,
    objectiveCards: List[ObjectiveCard] = List.empty
):

    def getPlayerState(playerId: String): Option[PlayerState] =
        states.find(_.playerId == turnManager.currentPlayer.id)

    def updatePlayerState(playerId: String, newState: PlayerState): GameState = 
        copy(states = states.map:
            case player if player.playerId == playerId => newState
            case player => player
        )

    def updateDecksManager(newDecksManager: DecksManager): GameState = copy(decksManager = newDecksManager)

    def updateBoard(newBoard: Board): GameState = copy(board = newBoard)

    def updateTurnManager(newTurnManager: TurnManager): GameState = copy(turnManager = newTurnManager)

    def checkWinCondition: Option[PlayerState] =
        states.find(player => player.objectiveCard.exists(
            obj => ObjectiveValidator.isCompleted(obj, this, player)
            )).map(_.playerId).flatMap(
                id => states.find(_.playerId == id))