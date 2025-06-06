package client

import spray.json._
import DefaultJsonProtocol._
import scalafx.scene.input.KeyCode.O

/**
 * Fornisce supporto per serializzazione/deserializzazione dei messaggi JSON lato client
 */
object ClientJsonSupport extends DefaultJsonProtocol:
  // Definizione dei messaggi che il client può inviare
  case class CreateGameMessage(gameName: String, maxPlayers: Int)
  case class JoinGameMessage(gameId: String)
  case class JoinLobbyMessage()
  case class LeaveGameMessage(gameId: String)
  
  // Definizione dei messaggi che il client può ricevere
  case class GameCreatedMessage(gameId: String, gameName: String, creatorId: String)
  case class PlayerJoinedMessage(gameId: String, playerId: String)
  case class ErrorMessage(message: String)
  case class LobbyJoinedMessage(message: String)
  case class GameJoinedMessage(gameId: String, players: List[String])
  case class LoginResponse(playerId: String, message: Option[String] = None)

  // Formati per i messaggi in uscita
  implicit val createGameFormat: RootJsonFormat[CreateGameMessage] = jsonFormat2(CreateGameMessage)
  implicit val joinGameFormat: RootJsonFormat[JoinGameMessage] = jsonFormat1(JoinGameMessage)
  implicit val joinLobbyFormat: RootJsonFormat[JoinLobbyMessage] = jsonFormat0(JoinLobbyMessage)
  implicit val leaveGameFormat: RootJsonFormat[LeaveGameMessage] = jsonFormat1(LeaveGameMessage)
  
  // Formati per i messaggi in entrata
  implicit val gameCreatedFormat: RootJsonFormat[GameCreatedMessage] = jsonFormat3(GameCreatedMessage)
  implicit val playerJoinedFormat: RootJsonFormat[PlayerJoinedMessage] = jsonFormat2(PlayerJoinedMessage)
  implicit val errorFormat: RootJsonFormat[ErrorMessage] = jsonFormat1(ErrorMessage)
  implicit val lobbyJoinedFormat: RootJsonFormat[LobbyJoinedMessage] = jsonFormat1(LobbyJoinedMessage)
  implicit val gameJoinedFormat: RootJsonFormat[GameJoinedMessage] = jsonFormat2(GameJoinedMessage)
  implicit val loginResponseFormat: RootJsonFormat[LoginResponse] = jsonFormat2(LoginResponse)

  // Metodo per serializzare i messaggi in uscita con il campo 'type'
  def toJson(message: Any): JsValue = message match
    case msg: CreateGameMessage => 
      JsObject(
        "type" -> JsString("createGame"),
        "gameName" -> JsString(msg.gameName),
        "maxPlayers" -> JsNumber(msg.maxPlayers)
      )
    case msg: JoinGameMessage => 
      JsObject(
        "type" -> JsString("joinGame"),
        "gameId" -> JsString(msg.gameId)
      )
    case _: JoinLobbyMessage => 
      JsObject("type" -> JsString("joinGame"), "gameId" -> JsString(""))
    case msg: LeaveGameMessage => 
      JsObject(
        "type" -> JsString("leaveGame"),
        "gameId" -> JsString(msg.gameId)
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
          GameJoinedMessage(gameId, players)
          
        case Some(JsString(unknownType)) => 
          ErrorMessage(s"Tipo di messaggio sconosciuto: $unknownType")
          
        case _ => 
          ErrorMessage("Messaggio non valido: campo 'type' mancante")
    catch
      case ex: Exception => 
        ErrorMessage(s"Errore durante il parsing JSON: ${ex.getMessage}")

end ClientJsonSupport
