package protocol

import spray.json._
import DefaultJsonProtocol._
import protocol.{Message => ProtocolMessage}

/**
 * Fornisce supporto per serializzazione/deserializzazione dei messaggi
 */
object JsonSupport extends DefaultJsonProtocol:
  // Formato per i messaggi client 
  implicit val createGameFormat: RootJsonFormat[ClientMessages.CreateGame] = jsonFormat2(ClientMessages.CreateGame)
  implicit val joinGameFormat: RootJsonFormat[ClientMessages.JoinGame] = jsonFormat1(ClientMessages.JoinGame)
  implicit val leaveGameFormat: RootJsonFormat[ClientMessages.LeaveGame] = jsonFormat1(ClientMessages.LeaveGame)
  
  // Formato per i messaggi server 
  implicit val gameCreatedFormat: RootJsonFormat[ServerMessages.GameCreated] = jsonFormat3(ServerMessages.GameCreated)
  implicit val playerJoinedFormat: RootJsonFormat[ServerMessages.PlayerJoined] = jsonFormat2(ServerMessages.PlayerJoined)
  implicit val errorFormat: RootJsonFormat[ServerMessages.Error] = jsonFormat1(ServerMessages.Error)
  implicit val lobbyJoinedFormat: RootJsonFormat[ServerMessages.LobbyJoined] = jsonFormat1(ServerMessages.LobbyJoined)

  // Lettura di messaggi client generici (con discriminator)
  implicit object ClientMessageJsonFormat extends RootJsonReader[ProtocolMessage]:
    def read(json: JsValue): ProtocolMessage =
      val fields = json.asJsObject.fields
      
      fields.get("type") match
        case Some(JsString("createGame")) => 
          // Estrai nome e max giocatori
          val name = fields.getOrElse("gameName", JsString("Nuova Partita")).convertTo[String]
          val maxPlayers = fields.getOrElse("maxPlayers", JsNumber(4)).convertTo[Int]
          ClientMessages.CreateGame(name, maxPlayers)
          
        case Some(JsString("joinGame")) => 
          // Estrai gameId
          val gameId = fields.getOrElse("gameId", JsString("")).convertTo[String]
          ClientMessages.JoinGame(gameId)
          
        case Some(JsString("JOIN_LOBBY")) => 
          // Caso speciale per JOIN_LOBBY
          ClientMessages.JoinGame("")
          
        case Some(JsString("leaveGame")) => 
          val gameId = fields.getOrElse("gameId", JsString("")).convertTo[String]
          ClientMessages.LeaveGame(gameId)
          
        case Some(JsString(unknown)) => 
          throw DeserializationException(s"Tipo di messaggio non riconosciuto: $unknown")
          
        case _ => 
          throw DeserializationException("Campo 'type' mancante")
  
  //Scrittura di messaggi server generici
  implicit object ServerMessageJsonFormat extends RootJsonWriter[ProtocolMessage]:
    def write(obj: ProtocolMessage): JsValue =
      obj match
        case msg: ServerMessages.GameCreated =>
          JsObject(
            "type" -> JsString("gameCreated"),
            "gameId" -> JsString(msg.gameId),
            "gameName" -> JsString(msg.gameName),
            "creatorId" -> JsString(msg.creatorId)
          )
          
        case msg: ServerMessages.PlayerJoined =>
          JsObject(
            "type" -> JsString("playerJoined"),
            "gameId" -> JsString(msg.gameId),
            "playerId" -> JsString(msg.playerId)
          )
          
        case msg: ServerMessages.Error =>
          JsObject(
            "type" -> JsString("error"),
            "message" -> JsString(msg.message)
          )

        case msg: ServerMessages.LobbyJoined =>
          JsObject(
            "type" -> JsString("lobbyJoined"),
            "message" -> JsString(msg.message)
          )
          
        case _ =>
          JsObject("type" -> JsString("unknown"))
  
  // Conversione implicita per permettere l'uso di toJson e convertTo
  implicit def messageToJson(msg: ProtocolMessage): JsValue = ServerMessageJsonFormat.write(msg)
  implicit def jsonToMessage(json: JsValue): ProtocolMessage = ClientMessageJsonFormat.read(json)

end JsonSupport

