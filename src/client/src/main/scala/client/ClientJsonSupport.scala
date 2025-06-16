package client

import spray.json._
import DefaultJsonProtocol._
import scalafx.scene.input.KeyCode.O

/**
 * Fornisce supporto per serializzazione/deserializzazione dei messaggi JSON lato client
 */
object ClientJsonSupport extends DefaultJsonProtocol:
  // Definizione dei messaggi che il client può inviare
  case class CreateGameMessage(gameName: String, maxPlayers: Int, username: String)
  case class JoinGameMessage(gameId: String, username: String)
  case class JoinLobbyMessage()
  case class LeaveGameMessage(gameId: String)
  case class GetAllGamesMessage() 
  case class PongMessage() // Risposta ai ping del server

  // Definizione dei messaggi che il client può ricevere
  case class GameCreatedMessage(gameId: String, gameName: String, creatorId: String)
  case class PlayerJoinedMessage(gameId: String, playerId: String)
  case class ErrorMessage(message: String)
  case class LobbyJoinedMessage(message: String)
  case class GameJoinedMessage(gameId: String, players: List[String], gameName: String)
  case class LoginResponse(playerId: String, message: Option[String] = None)
  case class PingMessage() 
  case class GameListMessage(games: List[String]) 

  // Formati per i messaggi in uscita
  implicit val createGameFormat: RootJsonFormat[CreateGameMessage] = jsonFormat3(CreateGameMessage)
  implicit val joinGameFormat: RootJsonFormat[JoinGameMessage] = jsonFormat2(JoinGameMessage)
  implicit val joinLobbyFormat: RootJsonFormat[JoinLobbyMessage] = jsonFormat0(JoinLobbyMessage)
  implicit val leaveGameFormat: RootJsonFormat[LeaveGameMessage] = jsonFormat1(LeaveGameMessage)
  implicit val getAllGamesFormat: RootJsonFormat[GetAllGamesMessage] = jsonFormat0(GetAllGamesMessage) 
  implicit val pingFormat: RootJsonFormat[PingMessage] = jsonFormat0(PingMessage)

  // Formati per i messaggi in entrata
  implicit val gameCreatedFormat: RootJsonFormat[GameCreatedMessage] = jsonFormat3(GameCreatedMessage)
  implicit val playerJoinedFormat: RootJsonFormat[PlayerJoinedMessage] = jsonFormat2(PlayerJoinedMessage)
  implicit val errorFormat: RootJsonFormat[ErrorMessage] = jsonFormat1(ErrorMessage)
  implicit val lobbyJoinedFormat: RootJsonFormat[LobbyJoinedMessage] = jsonFormat1(LobbyJoinedMessage)
  implicit val gameJoinedFormat: RootJsonFormat[GameJoinedMessage] = jsonFormat3(GameJoinedMessage)
  implicit val loginResponseFormat: RootJsonFormat[LoginResponse] = jsonFormat2(LoginResponse)
  implicit val pongFormat: RootJsonFormat[PongMessage] = jsonFormat0(PongMessage)
  implicit val gameListFormat: RootJsonFormat[GameListMessage] = jsonFormat1(GameListMessage)

  // Metodo per serializzare i messaggi in uscita con il campo 'type'
  def toJson(message: Any): JsValue = message match
    case msg: CreateGameMessage => 
      JsObject(
        "type" -> JsString("createGame"),
        "gameName" -> JsString(msg.gameName),
        "maxPlayers" -> JsNumber(msg.maxPlayers),
        "username" -> JsString(msg.username)
      )
    case msg: JoinGameMessage => 
      JsObject(
        "type" -> JsString("joinGame"),
        "gameId" -> JsString(msg.gameId),
        "username" -> JsString(msg.username)
      )
    case _: JoinLobbyMessage => 
      JsObject("type" -> JsString("joinGame"), "gameId" -> JsString(""))

    case msg: LeaveGameMessage => 
      JsObject(
        "type" -> JsString("leaveGame"),
        "gameId" -> JsString(msg.gameId)
      )
    
    case msg: GetAllGamesMessage => 
      JsObject("type" -> JsString("getAllGames"))
    

    case _: PongMessage => 
      JsObject("type" -> JsString("pong"))
      
    case msg: GameJoinedMessage =>
      JsObject(
        "type" -> JsString("gameJoined"),
        "gameId" -> JsString(msg.gameId),
        "players" -> JsArray(msg.players.map(JsString(_)).toVector)
      )
    case _ => JsObject("type" -> JsString("unknown"))
  
  // Metodo per deserializzare i messaggi in entrata
  def fromJson(json: String): Any = 
    try
      val jsValue = json.parseJson
      val fields = jsValue.asJsObject.fields
      
      fields.get("type") match
        case Some(JsString("gameCreated")) => 
          val gameId = fields.getOrElse("gameId", JsString("")).convertTo[String]
          val gameName = fields.getOrElse("gameName", JsString("")).convertTo[String]
          val creatorId = fields.getOrElse("creatorId", JsString("")).convertTo[String]
          GameCreatedMessage(gameId, gameName, creatorId)
          
        case Some(JsString("playerJoined")) => 
          val gameId = fields.getOrElse("gameId", JsString("")).convertTo[String]
          val playerId = fields.getOrElse("playerId", JsString("")).convertTo[String]
          PlayerJoinedMessage(gameId, playerId)
          
        case Some(JsString("error")) => 
          val message = fields.getOrElse("message", JsString("Errore sconosciuto")).convertTo[String]
          ErrorMessage(message)
          
        case Some(JsString("lobbyJoined")) => 
          val message = fields.getOrElse("message", JsString("")).convertTo[String]
          LobbyJoinedMessage(message)
          
        case Some(JsString("gameJoined")) => 
          val gameId = fields.getOrElse("gameId", JsString("")).convertTo[String]
          val players = fields.getOrElse("players", JsArray()).convertTo[List[String]]
          val gameName = fields.getOrElse("gameName", JsString("")).convertTo[String]
          GameJoinedMessage(gameId, players, gameName)

        case Some(JsString("gameList")) => 
          val games = fields.getOrElse("games", JsArray()).convertTo[List[String]]
          GameListMessage(games)
          
        case Some(JsString("ping")) => 
          PingMessage()
          
        case Some(JsString(unknownType)) => 
          ErrorMessage(s"Tipo di messaggio sconosciuto: $unknownType")
          
        case _ => 
          ErrorMessage("Messaggio non valido: campo 'type' mancante")
    catch
      case ex: Exception => 
        ErrorMessage(s"Errore durante il parsing JSON: ${ex.getMessage}")

end ClientJsonSupport
