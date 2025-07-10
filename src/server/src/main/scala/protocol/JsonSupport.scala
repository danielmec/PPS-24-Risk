package protocol

import spray.json._
import DefaultJsonProtocol._
import protocol.{Message => ProtocolMessage}
import server.GameSession.MissionCardDto
import server.GameSession.TerritoryDto
import server.GameSession.PlayerStateDto
import server.GameSession.GameStateDto
import server.GameSession.TerritoryCardDto

object JsonSupport extends DefaultJsonProtocol:
  implicit val createGameFormat: RootJsonFormat[ClientMessages.CreateGame] = jsonFormat6(protocol.ClientMessages.CreateGame.apply)
  implicit val joinGameFormat: RootJsonFormat[ClientMessages.JoinGame] = jsonFormat2(protocol.ClientMessages.JoinGame.apply)
  implicit val leaveGameFormat: RootJsonFormat[ClientMessages.LeaveGame] = jsonFormat1(protocol.ClientMessages.LeaveGame.apply)
  implicit val pongFormat: RootJsonFormat[ClientMessages.Pong] = jsonFormat0(protocol.ClientMessages.Pong.apply)
  implicit val getAllGamesFormat: RootJsonFormat[ClientMessages.GetAllGames] = jsonFormat0(protocol.ClientMessages.GetAllGames.apply)
  implicit val gameActionFormat: RootJsonFormat[ClientMessages.GameAction] = jsonFormat3(protocol.ClientMessages.GameAction.apply)
  implicit val gameCreatedFormat: RootJsonFormat[ServerMessages.GameCreated] = jsonFormat3(protocol.ServerMessages.GameCreated.apply)
  implicit val playerJoinedFormat: RootJsonFormat[ServerMessages.PlayerJoined] = jsonFormat2(protocol.ServerMessages.PlayerJoined.apply)
  implicit val errorFormat: RootJsonFormat[ServerMessages.Error] = jsonFormat1(protocol.ServerMessages.Error.apply)
  implicit val lobbyJoinedFormat: RootJsonFormat[ServerMessages.LobbyJoined] = jsonFormat1(protocol.ServerMessages.LobbyJoined.apply)
  implicit val pingFormat: RootJsonFormat[ServerMessages.Ping] = jsonFormat0(protocol.ServerMessages.Ping.apply)
  implicit val GameListFormat: RootJsonFormat[ServerMessages.GameList] = jsonFormat1(protocol.ServerMessages.GameList.apply)
  implicit val territoryDtoFormat: RootJsonFormat[TerritoryDto] = jsonFormat3(server.GameSession.TerritoryDto.apply)
  implicit val territoryCardDtoFormat: RootJsonFormat[TerritoryCardDto] = jsonFormat3(server.GameSession.TerritoryCardDto.apply)
  implicit val missionCardDtoFormat: RootJsonFormat[MissionCardDto] = jsonFormat4(server.GameSession.MissionCardDto.apply)
  implicit val playerStateDtoFormat: RootJsonFormat[PlayerStateDto] = jsonFormat5(server.GameSession.PlayerStateDto.apply)
  implicit val gameStateDtoFormat: RootJsonFormat[GameStateDto] = jsonFormat6(server.GameSession.GameStateDto.apply)

  implicit object MissionCardDtoOptionFormat extends RootJsonFormat[Option[MissionCardDto]] {
    def write(option: Option[MissionCardDto]): JsValue = option match {
      case Some(mission) => mission.toJson
      case None => JsNull
    }
    def read(json: JsValue): Option[MissionCardDto] = json match {
      case JsNull => None
      case js => Some(js.convertTo[MissionCardDto])
    }
  }

  implicit val gameSetupStartedFormat: RootJsonFormat[ServerMessages.GameSetupStarted] = jsonFormat2(protocol.ServerMessages.GameSetupStarted.apply)
  implicit object GameStateFormat extends RootJsonFormat[ServerMessages.GameState] {
    def write(gs: ServerMessages.GameState): JsValue = {
      JsObject(
        "gameId" -> JsString(gs.gameId),
        "players" -> JsArray(gs.players.map(JsString(_)).toVector),
        "currentPlayer" -> JsString(gs.currentPlayer),
        "state" -> mapToJson(gs.state)
      )
    }
    def read(json: JsValue): ServerMessages.GameState = {
      val fields = json.asJsObject.fields
      val gameId = fields("gameId").convertTo[String]
      val players = fields("players") match {
        case JsArray(values) => values.map(_.convertTo[String]).toList
        case _ => List.empty[String]
      }
      val currentPlayer = fields("currentPlayer").convertTo[String]
      val state = fields.getOrElse("state", JsObject.empty) match {
        case obj: JsObject => jsonToMap(obj)
        case _ => Map.empty[String, Any]
      }
      ServerMessages.GameState(gameId, players, currentPlayer, state)
    }
    private def jsonToMap(json: JsObject): Map[String, Any] = {
      json.fields.map { 
        case (key, JsString(value)) => key -> value
        case (key, JsNumber(value)) => key -> value.toInt
        case (key, JsBoolean(value)) => key -> value
        case (key, JsArray(elements)) => key -> elements.map {
          case JsString(s) => s
          case JsNumber(n) => n.toInt
          case JsBoolean(b) => b
          case obj: JsObject => jsonToMap(obj)
          case _ => null
        }.toList
        case (key, obj: JsObject) => key -> jsonToMap(obj)
        case (key, _) => key -> null
      }.toMap
    }
  }

  implicit val turnChangedFormat: RootJsonFormat[ServerMessages.TurnChanged] = jsonFormat3(protocol.ServerMessages.TurnChanged.apply)
  implicit val territoryUpdateFormat: RootJsonFormat[ServerMessages.TerritoryUpdate] = jsonFormat4(protocol.ServerMessages.TerritoryUpdate.apply)
  implicit val battleResultFormat: RootJsonFormat[ServerMessages.BattleResult] = jsonFormat8(protocol.ServerMessages.BattleResult.apply)
  implicit val gameOverFormat: RootJsonFormat[ServerMessages.GameOver] = jsonFormat3(protocol.ServerMessages.GameOver.apply)
  implicit val troopMovementFormat: RootJsonFormat[ServerMessages.TroopMovement] = jsonFormat5(protocol.ServerMessages.TroopMovement.apply)

  implicit object ClientMessageJsonFormat extends RootJsonReader[ProtocolMessage]:
    def read(json: JsValue): ProtocolMessage =
      val fields = json.asJsObject.fields
      fields.get("type") match
        case Some(JsString("createGame")) => 
          val name = fields.getOrElse("gameName", JsString("Nuova Partita")).convertTo[String]
          val maxPlayers = fields.getOrElse("maxPlayers", JsNumber(4)).convertTo[Int]
          val username = fields.getOrElse("username", JsString("player1")).convertTo[String]
          val numBots = fields.getOrElse("numBots", JsNumber(0)).convertTo[Int]
          val botStrategies = fields.get("botStrategies").map {
            case JsArray(types) => types.map {
              case JsString(t) => t
              case _ => "Offensivo" 
            }.toList
            case _ => List.empty[String]
          }
          val botNames = fields.get("botNames").map {
            case JsArray(names) => names.map {
              case JsString(n) => n
              case _ => "Bot" 
            }.toList
            case _ => List.empty[String]
          }
          ClientMessages.CreateGame(name, maxPlayers, username, numBots, botStrategies, botNames)
        case Some(JsString("joinGame")) => 
          val gameId = fields.getOrElse("gameId", JsString("")).convertTo[String]
          val username = fields.getOrElse("username", JsString("")).convertTo[String]
          ClientMessages.JoinGame(gameId,username)
        case Some(JsString("leaveGame")) => 
          val gameId = fields.getOrElse("gameId", JsString("")).convertTo[String]
          ClientMessages.LeaveGame(gameId)
        case Some(JsString("pong")) => 
          ClientMessages.Pong()
        case Some(JsString("joinLobby")) => 
          ClientMessages.JoinGame("","")
        case Some(JsString("getAllGames")) =>
          ClientMessages.GetAllGames()
        case Some(JsString("gameAction")) =>
          val gameId = fields.getOrElse("gameId", JsString("")).convertTo[String]
          val action = fields.getOrElse("action", JsString("")).convertTo[String]
          val params = fields.getOrElse("parameters", JsObject.empty) match {
            case obj: JsObject => obj.fields.map { case (k, v) => 
              k -> (v match {
                case JsString(s) => s
                case JsNumber(n) => n.toString
                case _ => ""
              })
            }
            case _ => Map.empty[String, String]
          }
          ClientMessages.GameAction(gameId, action, params)
        case Some(JsString(unknown)) => 
          throw DeserializationException(s"Tipo di messaggio non riconosciuto: $unknown")
        case _ => 
          throw DeserializationException("Campo 'type' mancante")
  
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
        case _: ServerMessages.Ping =>
          JsObject("type" -> JsString("ping"))
        case msg: ServerMessages.GameJoined =>
          // Prepara tutti i campi in un'unica mappa
          val fields = Map(
            "type" -> JsString("gameJoined"),
            "gameId" -> JsString(msg.gameId),
            "players" -> JsArray(msg.players.map(JsString(_)).toVector),
            "gameName" -> JsString(msg.gameName)
          ) ++ msg.playerColors.map(colors => 
              "playerColors" -> JsObject(colors.map { case (k, v) => k -> JsString(v) })
            ).toMap
          
          // Crea l'oggetto JSON
          JsObject(fields)
        case msg: ServerMessages.GameList =>
          JsObject(
            "type" -> JsString("gameList"),
            "games" -> JsArray(msg.games.map(JsString(_)).toVector)
          )
        case msg: ServerMessages.GameRemoved =>
          JsObject(
            "type" -> JsString("gameRemoved"),
            "gameId" -> JsString(msg.gameId)
          )
        case msg: ServerMessages.GameSetupStarted =>
          JsObject(
            "type" -> JsString("gameSetupStarted"),
            "gameId" -> JsString(msg.gameId),
            "message" -> JsString(msg.message)  
          )
        case msg: ServerMessages.GameStarted =>
          val gameStateDto = msg.initialState.getOrElse("gameStateDto", null).asInstanceOf[GameStateDto]
          JsObject(
            "type" -> JsString("gameStarted"),
            "gameId" -> JsString(msg.gameId),
            "currentPlayerId" -> JsString(msg.currentPlayerId),
            "initialState" -> JsObject(
              "gameId" -> JsString(msg.gameId),
              "players" -> JsArray(msg.initialState.getOrElse("players", List.empty[String]).asInstanceOf[List[String]].map(JsString(_)).toVector),
              "currentPlayer" -> JsString(msg.currentPlayerId),
              "state" -> (if (gameStateDto != null) gameStateDto.toJson else JsObject())
            )
          )
        case msg: ServerMessages.TurnChanged =>
          JsObject(
            "type" -> JsString("turnChanged"),
            "gameId" -> JsString(msg.gameId),
            "playerId" -> JsString(msg.playerId),  
            "phase" -> JsString(msg.phase) 
          )
        case msg: ServerMessages.TerritoryUpdate =>
          JsObject(
            "type" -> JsString("territoryUpdate"),
            "gameId" -> JsString(msg.gameId),
            "territoryName" -> JsString(msg.territoryName),  
            "owner" -> JsString(msg.owner),
            "troops" -> JsNumber(msg.troops) 
          )
        case msg: ServerMessages.BattleResult =>
          JsObject(
            "type" -> JsString("battleResult"),
            "gameId" -> JsString(msg.gameId),
            "attackerTerritory" -> JsString(msg.attackerTerritory),  
            "defenderTerritory" -> JsString(msg.defenderTerritory),
            "attackerDice" -> JsArray(msg.attackerDice.map(JsNumber(_)).toVector),
            "defenderDice" -> JsArray(msg.defenderDice.map(JsNumber(_)).toVector),  
            "attackerLosses" -> JsNumber(msg.attackerLosses),  
            "defenderLosses" -> JsNumber(msg.defenderLosses),  
            "conquered" -> JsBoolean(msg.conquered)
          )
        case msg: ServerMessages.GameOver =>
          JsObject(
            "type" -> JsString("gameOver"),
            "gameId" -> JsString(msg.gameId),
            "winnerId" -> JsString(msg.winnerId),  
            "winnerUsername" -> JsString(msg.winnerUsername)  
          )
        case msg: ServerMessages.GameState =>
          val stateMap = msg.state
          val gameStateDto = stateMap.getOrElse("gameStateDto", null).asInstanceOf[GameStateDto]
          JsObject(
            "type" -> JsString("gameState"),
            "gameId" -> JsString(msg.gameId),
            "players" -> JsArray(msg.players.map(JsString(_)).toVector),
            "currentPlayer" -> JsString(msg.currentPlayer),
            "state" -> gameStateDto.toJson
          )
        case msg: ServerMessages.GameActionResult =>
          JsObject(
            "type" -> JsString("gameActionResult"),
            "success" -> JsBoolean(msg.success),
            "message" -> JsString(msg.message)
          )
        case msg: ServerMessages.TrisPlayed =>
          JsObject(
            "type" -> JsString("trisPlayed"),
            "gameId" -> JsString(msg.gameId),
            "playerId" -> JsString(msg.playerId),
            "bonus" -> JsNumber(msg.bonus)
          )
        case msg: ServerMessages.PlayerLeft =>
          JsObject(
            "type" -> JsString("playerLeft"),
            "gameId" -> JsString(msg.gameId),
            "playerId" -> JsString(msg.playerId)
          )
        case _ =>
          println(s"[WARN] Messaggio di tipo sconosciuto: ${obj.getClass.getName}")
          JsObject("type" -> JsString("unknown"))
  
  implicit def messageToJson(msg: ProtocolMessage): JsValue = ServerMessageJsonFormat.write(msg)

  private def mapToJson(map: Map[String, Any]): JsObject =
    JsObject(map.map {
      case (key, value: String) => key -> JsString(value)
      case (key, value: Int) => key -> JsNumber(value)
      case (key, value: Long) => key -> JsNumber(value)
      case (key, value: Double) => key -> JsNumber(value)
      case (key, value: Boolean) => key -> JsBoolean(value)
      case (key, value: List[_]) => key -> JsArray(value.map {
        case s: String => JsString(s)
        case n: Int => JsNumber(n)
        case n: Long => JsNumber(n)
        case n: Double => JsNumber(n)
        case b: Boolean => JsBoolean(b)
        case m: Map[String, Any] @unchecked => mapToJson(m)
        case _ => JsNull
      }.toVector)
      case (key, value: Map[String, Any] @unchecked) => key -> mapToJson(value)
      case (key, _) => key -> JsNull
    })

end JsonSupport

