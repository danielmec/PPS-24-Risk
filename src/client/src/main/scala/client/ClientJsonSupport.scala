package client

import spray.json._
import DefaultJsonProtocol._


object ClientJsonSupport extends DefaultJsonProtocol:
  case class CreateGameMessage(
    gameName: String, 
    maxPlayers: Int, 
    username: String, 
    numBots: Int = 0, 
    botStrategies: Option[List[String]] = None, 
    botNames: Option[List[String]] = None
  )
  case class JoinGameMessage(gameId: String, username: String)
  case class JoinLobbyMessage()
  case class LeaveGameMessage(gameId: String)
  case class GetAllGamesMessage() 
  case class PongMessage()
  case class GameActionMessage(gameId: String, action: String, parameters: Map[String, String])

  case class GameCreatedMessage(gameId: String, gameName: String, creatorId: String)
  case class PlayerJoinedMessage(gameId: String, playerId: String)
  case class ErrorMessage(message: String)
  case class LobbyJoinedMessage(message: String)
  case class GameJoinedMessage(gameId: String, players: List[String], gameName: String, playerColors: Option[Map[String, String]] = None)
  case class LoginResponse(playerId: String, message: Option[String] = None)
  case class PingMessage() 
  case class GameListMessage(games: List[String])
  
  case class GameSetupStartedMessage(gameId: String, message: String)
  case class PlayerLeftMessage(gameId: String, player: String)
  
  case class Territory(name: String, owner: String, troops: Int)
  case class PlayerState(playerId: String, cards: Int, bonusTroops: Int)
  case class MissionCardDto(id: String, description: String, targetType: String, targetValue: String)

  case class BattleResultMessage(
    gameId: String,
    attackerTerritory: String,
    defenderTerritory: String,
    attackerDice: List[Int],
    defenderDice: List[Int],
    attackerLosses: Int,
    defenderLosses: Int,
    conquered: Boolean
  )

  case class TroopMovementMessage(
    gameId: String,
    fromTerritory: String,
    toTerritory: String,
    troops: Int,
    playerId: String
  )
  
  case class GameStateData(
    currentPlayer: String,
    currentPhase: String,
    territories: List[Map[String, String]],
    playerStates: List[Map[String, String]],
    playerStartedTurn: String = "false" 
  )
  
  case class GameState(
    gameId: String,
    players: List[String],
    currentPlayer: String,
    state: GameStateData
  )
  
  case class GameStartedMessage(
    gameId: String,
    currentPlayerId: String,
    initialState: GameState
  )
  
  case class GameActionResultMessage(success: Boolean, message: String)
  case class TurnChangedMessage(gameId: String, playerId: String, phase: String)
  case class TerritoryUpdateMessage(gameId: String, territoryName: String, owner: String, troops: Int)
  case class TrisPlayedMessage(gameId: String, playerId: String, bonus: Int)
  case class GameOverMessage(gameId: String, winnerId: String, winnerUsername: String)

  private def extractString(fields: Map[String, JsValue], key: String, default: String = ""): String =
    fields.getOrElse(key, JsString(default)).convertTo[String]
    
  private def extractInt(fields: Map[String, JsValue], key: String, default: Int = 0): Int =
    fields.getOrElse(key, JsNumber(default)).convertTo[Int]
    
  private def extractBoolean(fields: Map[String, JsValue], key: String, default: Boolean = false): Boolean =
    fields.getOrElse(key, JsBoolean(default)).convertTo[Boolean]
    
  private def extractStringList(fields: Map[String, JsValue], key: String): List[String] =
    fields.getOrElse(key, JsArray()).convertTo[List[String]]
    
  private def extractOption[T](fields: Map[String, JsValue], key: String)(convert: JsValue => T): Option[T] =
    fields.get(key) match
      case Some(JsNull) => None
      case Some(value) => Some(convert(value))
      case None => None
      
  private def handleMissionCard(json: JsObject): Map[String, String] =
    json.fields.map {
      case (k, v: JsObject) if k == "missionCard" => 
        val description = v.fields.getOrElse("description", JsString(""))
        k -> description.convertTo[String]
      case (k, JsString(s)) => k -> s
      case (k, JsNumber(n)) => k -> n.toString
      case (k, JsBoolean(b)) => k -> b.toString
      case (k, JsNull) => k -> ""
      case (k, v) => k -> v.toString.replace("\"", "")
    }

  implicit val createGameFormat: RootJsonFormat[CreateGameMessage] = jsonFormat6(CreateGameMessage.apply)
  implicit val joinGameFormat: RootJsonFormat[JoinGameMessage] = jsonFormat2(JoinGameMessage.apply)
  implicit val joinLobbyFormat: RootJsonFormat[JoinLobbyMessage] = jsonFormat0(JoinLobbyMessage.apply)
  implicit val leaveGameFormat: RootJsonFormat[LeaveGameMessage] = jsonFormat1(LeaveGameMessage.apply)
  implicit val getAllGamesFormat: RootJsonFormat[GetAllGamesMessage] = jsonFormat0(GetAllGamesMessage.apply) 
  implicit val pingFormat: RootJsonFormat[PingMessage] = jsonFormat0(PingMessage.apply)
  implicit val gameActionFormat: RootJsonFormat[GameActionMessage] = jsonFormat3(GameActionMessage.apply)
  implicit val gameCreatedFormat: RootJsonFormat[GameCreatedMessage] = jsonFormat3(GameCreatedMessage.apply)
  implicit val playerJoinedFormat: RootJsonFormat[PlayerJoinedMessage] = jsonFormat2(PlayerJoinedMessage.apply)
  implicit val errorFormat: RootJsonFormat[ErrorMessage] = jsonFormat1(ErrorMessage.apply)
  implicit val lobbyJoinedFormat: RootJsonFormat[LobbyJoinedMessage] = jsonFormat1(LobbyJoinedMessage.apply)
  implicit val gameJoinedFormat: RootJsonFormat[GameJoinedMessage] = jsonFormat4(GameJoinedMessage.apply)
  implicit val loginResponseFormat: RootJsonFormat[LoginResponse] = jsonFormat2(LoginResponse.apply)
  implicit val pongFormat: RootJsonFormat[PongMessage] = jsonFormat0(PongMessage.apply)
  implicit val gameListFormat: RootJsonFormat[GameListMessage] = jsonFormat1(GameListMessage.apply)
  implicit val gameSetupStartedFormat: RootJsonFormat[GameSetupStartedMessage] = jsonFormat2(GameSetupStartedMessage.apply)
  implicit val playerLeftFormat: RootJsonFormat[PlayerLeftMessage] = jsonFormat2(PlayerLeftMessage.apply)
  implicit val missionCardDtoFormat: RootJsonFormat[MissionCardDto] = jsonFormat4(MissionCardDto.apply)
  
  implicit object GameStateDataFormat extends RootJsonFormat[GameStateData] {
    def write(obj: GameStateData): JsValue = {
      JsObject(
        "currentPlayer" -> JsString(obj.currentPlayer),
        "currentPhase" -> JsString(obj.currentPhase),
        "territories" -> JsArray(obj.territories.map(t => 
          JsObject(t.map { case (k, v) => k -> JsString(v) })
        )),
        "playerStates" -> JsArray(obj.playerStates.map(ps => 
          JsObject(ps.map { case (k, v) => k -> JsString(v) })
        )),
        "playerStartedTurn" -> JsString(obj.playerStartedTurn)
      )
    }
    
    def read(json: JsValue): GameStateData = {
      val fields = json.asJsObject.fields
      
      GameStateData(
        currentPlayer = extractString(fields, "currentPlayer"),
        currentPhase = extractString(fields, "currentPhase", "PlacingTroops"),
        territories = fields.getOrElse("territories", JsArray()).convertTo[List[JsObject]].map(
          _.fields.map { case (k, v) => k -> v.toString.replace("\"", "") }
        ),
        playerStates = fields.getOrElse("playerStates", JsArray()).convertTo[List[JsObject]].map { playerState =>
          handleMissionCard(playerState)
        },
        playerStartedTurn = extractString(fields, "playerStartedTurn", "false")
      )
    }
  }
  
  implicit val gameStateFormat: RootJsonFormat[GameState] = jsonFormat4(GameState.apply)
  implicit val gameStartedFormat: RootJsonFormat[GameStartedMessage] = jsonFormat3(GameStartedMessage.apply)
  implicit val gameActionResultFormat: RootJsonFormat[GameActionResultMessage] = jsonFormat2(GameActionResultMessage.apply)
  implicit val turnChangedFormat: RootJsonFormat[TurnChangedMessage] = jsonFormat3(TurnChangedMessage.apply)
  implicit val territoryUpdateFormat: RootJsonFormat[TerritoryUpdateMessage] = jsonFormat4(TerritoryUpdateMessage.apply)
  implicit val battleResultFormat: RootJsonFormat[BattleResultMessage] = jsonFormat8(BattleResultMessage.apply)
  implicit val troopMovementFormat: RootJsonFormat[TroopMovementMessage] = jsonFormat5(TroopMovementMessage.apply)
  implicit val gameOverFormat: RootJsonFormat[GameOverMessage] = jsonFormat3(GameOverMessage.apply)

  def toJson(message: Any): JsValue = message match
    case msg: CreateGameMessage => 
      var fields = Map(
        "type" -> JsString("createGame"),
        "gameName" -> JsString(msg.gameName),
        "maxPlayers" -> JsNumber(msg.maxPlayers),
        "username" -> JsString(msg.username),
        "numBots" -> JsNumber(msg.numBots)
      )
      
      if (msg.botStrategies.isDefined) 
        fields = fields + ("botStrategies" -> JsArray(
          msg.botStrategies.get.map(JsString(_)).toVector
        ))
      
      if (msg.botNames.isDefined) 
        fields = fields + ("botNames" -> JsArray(
          msg.botNames.get.map(JsString(_)).toVector
        ))
      
      JsObject(fields)
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
    case _: GetAllGamesMessage => 
      JsObject("type" -> JsString("getAllGames"))
    case _: PongMessage => 
      JsObject("type" -> JsString("pong"))
    case msg: GameActionMessage =>
      JsObject(
        "type" -> JsString("gameAction"),
        "gameId" -> JsString(msg.gameId),
        "action" -> JsString(msg.action),
        "parameters" -> JsObject(msg.parameters.map { case (k, v) => k -> JsString(v) })
      )
    case msg: GameJoinedMessage =>
      JsObject(
        "type" -> JsString("gameJoined"),
        "gameId" -> JsString(msg.gameId),
        "players" -> JsArray(msg.players.map(JsString(_)).toVector)
      )
    case _ => JsObject("type" -> JsString("unknown"))
  
  private val messageHandlers: Map[String, Map[String, JsValue] => Any] = Map(
    "gameCreated" -> { fields => 
      GameCreatedMessage(
        extractString(fields, "gameId"),
        extractString(fields, "gameName"),
        extractString(fields, "creatorId")
      )
    },
    
    "playerJoined" -> { fields => 
      PlayerJoinedMessage(
        extractString(fields, "gameId"),
        extractString(fields, "playerId")
      )
    },
    
    "error" -> { fields => 
      ErrorMessage(extractString(fields, "message", "Errore sconosciuto"))
    },
    
    "lobbyJoined" -> { fields => 
      LobbyJoinedMessage(extractString(fields, "message"))
    },
    
    "gameJoined" -> { fields => 
      GameJoinedMessage(
        extractString(fields, "gameId"),
        extractStringList(fields, "players"),
        extractString(fields, "gameName"),
        extractOption(fields, "playerColors")(_.convertTo[Map[String, String]])
      )
    },
    
    "gameList" -> { fields => 
      GameListMessage(extractStringList(fields, "games"))
    },
    
    "ping" -> { _ => PingMessage() },
    
    "gameSetupStarted" -> { fields => 
      GameSetupStartedMessage(
        extractString(fields, "gameId"),
        extractString(fields, "message")
      )
    },
    
    "gameStarted" -> { fields =>
      val gameId = extractString(fields, "gameId")
      val currentPlayerId = extractString(fields, "currentPlayerId")
      
      val initialStateJson = fields.getOrElse("initialState", JsObject.empty).asJsObject
      val initialStateFields = initialStateJson.fields
      
      val gameState = if (initialStateFields.contains("state")) {
        val stateJson = initialStateFields.getOrElse("state", JsObject.empty).asJsObject
        
        GameState(
          gameId = extractString(initialStateFields, "gameId", gameId),
          players = extractStringList(initialStateFields, "players"),
          currentPlayer = extractString(initialStateFields, "currentPlayer", currentPlayerId),
          state = stateJson.convertTo[GameStateData]  // Usa il formato personalizzato
        )
      } else {
        
        GameState(
          gameId = gameId,
          players = extractStringList(initialStateFields, "players"),
          currentPlayer = currentPlayerId,
          state = GameStateData(
            currentPlayer = currentPlayerId,
            currentPhase = "PlacingTroops",
            territories = List(),
            playerStates = List()
          )
        )
      }
      
      GameStartedMessage(gameId, currentPlayerId, gameState)
    },
    
    "gameState" -> { fields =>
      val gameId = extractString(fields, "gameId")
      val players = extractStringList(fields, "players")
      val currentPlayer = extractString(fields, "currentPlayer")
      
      val stateJson = fields.getOrElse("state", JsObject.empty).asJsObject
      val stateFields = stateJson.fields
      
      val gameStateData = if (stateFields.contains("gameStateDto") && stateFields("gameStateDto") != JsNull) {
        stateFields("gameStateDto").convertTo[GameStateData]
      } else {
        stateJson.convertTo[GameStateData]
      }
      
      GameState(gameId, players, currentPlayer, gameStateData)
    },
    
    "gameActionResult" -> { fields =>
      GameActionResultMessage(
        extractBoolean(fields, "success", false),
        extractString(fields, "message")
      )
    },
    
    "turnChanged" -> { fields =>
      TurnChangedMessage(
        extractString(fields, "gameId"),
        extractString(fields, "playerId"),
        extractString(fields, "phase")
      )
    },
    
    "territoryUpdate" -> { fields =>
      TerritoryUpdateMessage(
        extractString(fields, "gameId"),
        extractString(fields, "territoryName"),
        extractString(fields, "owner"),
        extractInt(fields, "troops")
      )
    },
    
    "battleResult" -> { fields =>
      BattleResultMessage(
        extractString(fields, "gameId"),
        extractString(fields, "attackerTerritory"),
        extractString(fields, "defenderTerritory"),
        fields.get("attackerDice").map(_.convertTo[List[Int]]).getOrElse(List.empty),
        fields.get("defenderDice").map(_.convertTo[List[Int]]).getOrElse(List.empty),
        extractInt(fields, "attackerLosses"),
        extractInt(fields, "defenderLosses"),
        extractBoolean(fields, "conquered")
      )
    },
    
    "troopMovement" -> { fields =>
      TroopMovementMessage(
        extractString(fields, "gameId"),
        extractString(fields, "fromTerritory"),
        extractString(fields, "toTerritory"),
        extractInt(fields, "troops"),
        extractString(fields, "playerId")
      )
    },
    
    "gameOver" -> { fields =>
      GameOverMessage(
        extractString(fields, "gameId"),
        extractString(fields, "winnerId"),
        extractString(fields, "winnerUsername")
      )
    },
    
    "playerLeft" -> { fields =>
      PlayerLeftMessage(
        extractString(fields, "gameId"),
        extractString(fields, "player")
      )
    },
    
    "trisPlayed" -> { fields =>
      TrisPlayedMessage(
        extractString(fields, "gameId"),
        extractString(fields, "playerId"),
        extractInt(fields, "bonus")
      )
    }
  )

  def fromJson(json: String): Any = 
    try
      val jsValue = json.parseJson
      val fields = jsValue.asJsObject.fields
      
      fields.get("type") match
        case Some(JsString(msgType)) if messageHandlers.contains(msgType) =>
          messageHandlers(msgType)(fields)
        
        case Some(JsString(unknownType)) => 
          ErrorMessage(s"Tipo di messaggio sconosciuto: $unknownType")
          
        case _ => 
          ErrorMessage("Messaggio non valido: campo 'type' mancante")
    catch
      case e: DeserializationException => 
        ErrorMessage(s"Errore di formato: ${e.getMessage}")
      case e: NoSuchElementException => 
        ErrorMessage(s"Campo mancante: ${e.getMessage}")
      case e: Exception => 
        ErrorMessage(s"Errore durante il parsing JSON: ${e.getMessage}")

end ClientJsonSupport
