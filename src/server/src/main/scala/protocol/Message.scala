package protocol

sealed trait Message {
    
  def messageType: String = this.getClass.getSimpleName.replace("$", "")
}

//classe Singleton per i messaggi dal cliente al server
object ClientMessages:

  case class Login(username: String ) extends Message

  case class JoinGame(gameId: String, username: String) extends Message

  case class CreateGame(
    gameName: String, 
    maxPlayers: Int,
    username: String,
    numBots: Int = 0,
    botStrategies: Option[List[String]] = None,
    botNames: Option[List[String]] = None
  ) extends Message

  case class GameAction(
    gameId: String,
    action: String,
    parameters: Map[String, String] 
  ) extends Message

  case class LeaveGame(gameId: String) extends Message

  case class GetAllGames() extends Message 
  

  case object Logout extends Message  

  case class Pong() extends Message {
    override val messageType: String = "pong"
  }


// classe Singleton per i messaggi dal server al client
object ServerMessages:
    case class LoginResponse(
        success: Boolean, 
        playerId:Option[String], 
        errorMessage: Option[String]
    ) extends Message

    case class GameCreated(
        gameId: String, 
        gameName: String,
        creatorId: String
    ) extends Message

    case class PlayerJoined(
        gameId: String, 
        playerId: String
    ) extends Message

    case class GameJoined(
        gameId: String, 
        players: List[String],
        gameName: String
    ) extends Message

    case class LobbyJoined(
        message: String
    ) extends Message

    case class GameState(
      gameId: String,
      players: List[String],  
      currentPlayer: String,  
      state: Map[String, Any] //stato del gioco generico
    ) extends Message

    case class GameActionResult(
        success: Boolean,
        message: String,
    ) extends Message

    case class GameList(games: List[String]) extends Message

    case class PlayerLeft(gameId: String, playerId:String) extends Message

    case class Error(message: String) extends Message

    case class Ping() extends Message {
      override val messageType: String = "ping"
    }
    case class GameRemoved(gameId: String) extends Message

    case class GameSetupStarted(
        gameId: String,
        message: String
    ) extends Message

    case class GameStarted(
        gameId: String,
        currentPlayerId: String,
        initialState: Map[String, Any]  
    ) extends Message

    case class TurnChanged(
        gameId: String,
        playerId: String,
        phase: String
    ) extends Message

    case class TerritoryUpdate(
        gameId: String,
        territoryName: String,
        owner: String,
        troops: Int
    ) extends Message

    case class BattleResult(
        gameId: String,
        attackerTerritory: String,
        defenderTerritory: String,
        attackerLosses: Int,
        defenderLosses: Int,
        newOwner: Option[String]
    ) extends Message

    case class GameOver(
        gameId: String,
        winnerId: String,
        winnerUsername: String
    ) extends Message