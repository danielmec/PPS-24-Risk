case class GameState(
    gameId: String,
    board: Board,
    states: List[PlayerState],
    turnManager: TurnManager,
    territoryCards: List[TerritoryCard] = List.empty,
    objectiveCards: List[ObjectiveCard] = List.empty
):

    def getPlayerState(playerId: String): Option[PlayerState] =
        states.find(_.playerId == turnManager.currentPlayer.playerId)

    def updatePlayerState(playerId: String, newState: PlayerState): GameState = 
        copy(states = states.map:
            case player if player.playerId == playerId => newState
            case player => player
        )

    def updateCardManager(newCardManager: CardManager): GameState = copy(cardManager = newCardManager)

    def updateBoard(newBoard: Board): GameState = copy(board = newBoard)

    def updateTurnManager(newTurnManager: TurnManager): GameState = copy(turnManager = newTurnManager)

    def checkWinCondition: Option[Player] =
        states.find(player => player.objective.exists(
            obj => ObjectiveValidator.isCompleted(obj, this, player)
            )).map(_.playerId).flatMap(
                id => states.find(_.playerId == id))