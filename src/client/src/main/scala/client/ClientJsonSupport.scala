package client

import spray.json._
import DefaultJsonProtocol._

/** 
 * Provides JSON serialization and deserialization support for client-server communication in the Risk game.
 * This object handles all message conversions between Scala objects and JSON for network communication.
 */
object ClientJsonSupport extends DefaultJsonProtocol:
  /**
   * Message sent to create a new game.
   *
   * @param gameName The name of the game to create
   * @param maxPlayers Maximum number of players allowed in the game
   * @param username Username of the game creator
   * @param numBots Number of bot players to add
   * @param botStrategies Optional list of strategy names for the bots
   * @param botNames Optional list of names for the bots
   */
  case class CreateGameMessage(
    gameName: String, 
    maxPlayers: Int, 
    username: String, 
    numBots: Int = 0, 
    botStrategies: Option[List[String]] = None, 
    botNames: Option[List[String]] = None
  )
  
  /** Message to join an existing game. */
  case class JoinGameMessage(gameId: String, username: String)
  
  /** Message to join the game lobby. */
  case class JoinLobbyMessage()
  
  /** Message to leave a game. */
  case class LeaveGameMessage(gameId: String)
  
  /** Message to request all available games. */
  case class GetAllGamesMessage() 
  
  /** Message to respond to a ping. */
  case class PongMessage()
  
  /** Message to send a game action with parameters. */
  case class GameActionMessage(gameId: String, action: String, parameters: Map[String, String])

  /** Message sent when a game is created. */
  case class GameCreatedMessage(gameId: String, gameName: String, creatorId: String)
  
  /** Message sent when a player joins a game. */
  case class PlayerJoinedMessage(gameId: String, playerId: String)
  
  /** Message sent when an error occurs. */
  case class ErrorMessage(message: String)
  
  /** Message sent when a player successfully joins the lobby. */
  case class LobbyJoinedMessage(message: String)
  
  /** Message sent when a player successfully joins a game. */
  case class GameJoinedMessage(gameId: String, players: List[String], gameName: String, playerColors: Option[Map[String, String]] = None)
  
  /** Response to a login attempt. */
  case class LoginResponse(playerId: String, message: Option[String] = None)
  
  /** Message sent to check connection. */
  case class PingMessage() 
  
  /** Message containing a list of available games. */
  case class GameListMessage(games: List[(String, String)])  // (gameId, gameName)
  
  /** Message sent when game setup phase has started. */
  case class GameSetupStartedMessage(gameId: String, message: String)
  
  /** Message sent when a player leaves a game. */
  case class PlayerLeftMessage(gameId: String, player: String)
  
  /** Represents a territory in the game. */
  case class Territory(name: String, owner: String, troops: Int)
  
  /** Represents a player's current state. */
  case class PlayerState(playerId: String, cards: Int, bonusTroops: Int)
  
  /** Data transfer object for mission cards. */
  case class MissionCardDto(id: String, description: String, targetType: String, targetValue: String)

  /** Message sent with the results of a battle. */
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

  /** Message sent when troops are moved between territories. */
  case class TroopMovementMessage(
    gameId: String,
    fromTerritory: String,
    toTerritory: String,
    troops: Int,
    playerId: String
  )
  
  /** Contains data about the current game state. */
  case class GameStateData(
    currentPlayer: String,
    currentPhase: String,
    territories: List[Map[String, String]],
    playerStates: List[Map[String, String]],
    playerStartedTurn: String = "false" 
  )
  
  /** Represents the complete state of a game. */
  case class GameState(
    gameId: String,
    players: List[String],
    currentPlayer: String,
    state: GameStateData
  )
  
  /** Message sent when a game starts. */
  case class GameStartedMessage(
    gameId: String,
    currentPlayerId: String,
    initialState: GameState
  )
  
  /** Message containing the result of a game action. */
  case class GameActionResultMessage(success: Boolean, message: String)
  
  /** Message sent when the turn changes to another player or phase. */
  case class TurnChangedMessage(gameId: String, playerId: String, phase: String)
  
  /** Message sent when a territory's state is updated. */
  case class TerritoryUpdateMessage(gameId: String, territoryName: String, owner: String, troops: Int)
  
  /** Message sent when a player plays a card tris. */
  case class TrisPlayedMessage(gameId: String, playerId: String, bonus: Int)
  
  /** Message sent when the game is over. */
  case class GameOverMessage(gameId: String, winnerId: String, winnerUsername: String)

  /**
   * Extracts a string value from a JSON fields map.
   *
   * @param fields The map of JSON fields
   * @param key The key to extract
   * @param default The default value if the key is not found
   * @return The extracted string value
   */
  private def extractString(fields: Map[String, JsValue], key: String, default: String = ""): String =
    fields.getOrElse(key, JsString(default)).convertTo[String]
    
  /**
   * Extracts an integer value from a JSON fields map.
   *
   * @param fields The map of JSON fields
   * @param key The key to extract
   * @param default The default value if the key is not found
   * @return The extracted integer value
   */
  private def extractInt(fields: Map[String, JsValue], key: String, default: Int = 0): Int =
    fields.getOrElse(key, JsNumber(default)).convertTo[Int]
    
  /**
   * Extracts a boolean value from a JSON fields map.
   *
   * @param fields The map of JSON fields
   * @param key The key to extract
   * @param default The default value if the key is not found
   * @return The extracted boolean value
   */
  private def extractBoolean(fields: Map[String, JsValue], key: String, default: Boolean = false): Boolean =
    fields.getOrElse(key, JsBoolean(default)).convertTo[Boolean]
    
  /**
   * Extracts a list of strings from a JSON fields map.
   *
   * @param fields The map of JSON fields
   * @param key The key to extract
   * @return The list of strings, or an empty list if not found
   * @throws DeserializationException if the value is not a valid array of strings
   */
  private def extractStringList(fields: Map[String, JsValue], key: String): List[String] =
    fields.get(key).map {
      case JsArray(elements) => elements.map {
        case JsString(s) => s
        case other => throw DeserializationException(s"Expected string element in array, but got $other")
      }.toList
      case other => throw DeserializationException(s"Expected array for field $key, but got $other")
    }.getOrElse(List.empty)
    
  /**
   * Extracts a list of game information tuples (gameId, gameName) from a JSON fields map.
   *
   * @param fields The map of JSON fields
   * @param key The key to extract
   * @return List of (gameId, gameName) tuples, or an empty list if not found
   * @throws DeserializationException if the value is not in the expected format
   */
  private def extractGameInfoList(fields: Map[String, JsValue], key: String): List[(String, String)] =
    fields.get(key).map {
      case JsArray(elements) => elements.map {
        case JsArray(Vector(JsString(id), JsString(name))) => (id, name)
        case other => throw DeserializationException(s"Expected tuple [id, name] in array, but got $other")
      }.toList
      case other => throw DeserializationException(s"Expected array for field $key, but got $other")
    }.getOrElse(List.empty)
    
  /**
   * Extracts an optional value from a JSON fields map.
   *
   * @param fields The map of JSON fields
   * @param key The key to extract
   * @param convert Function to convert the JSON value to the desired type
   * @return An Option containing the converted value, or None if not present or null
   */
  private def extractOption[T](fields: Map[String, JsValue], key: String)(convert: JsValue => T): Option[T] =
    fields.get(key) match
      case Some(JsNull) => None
      case Some(value) => Some(convert(value))
      case None => None
      
  /**
   * Handles mission card JSON objects, converting them to a map of string key-value pairs.
   *
   * @param json The JSON object containing mission card data
   * @return A map of string keys and values
   */
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

  // JSON format definitions for the case classes
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
  
  /**
   * Custom JSON format for GameStateData to handle special conversion needs.
   */
  implicit object GameStateDataFormat extends RootJsonFormat[GameStateData] {
    /**
     * Converts a GameStateData object to JSON.
     *
     * @param obj The GameStateData to convert
     * @return The JSON representation
     */
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
    
    /**
     * Converts JSON to a GameStateData object.
     *
     * @param json The JSON to convert
     * @return The GameStateData object
     */
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

  /**
   * Converts a message object to its JSON representation.
   *
   * @param message The message to convert
   * @return The JSON value representing the message
   */
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
  
  /**
   * Map of message type handlers for deserializing incoming messages.
   * Each handler takes a map of JSON fields and returns the appropriate message object.
   */
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
      GameListMessage(extractGameInfoList(fields, "games"))
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
        extractString(fields, "playerId")
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

  /**
   * Parses a JSON string into a message object.
   *
   * @param json The JSON string to parse
   * @return The corresponding message object, or an ErrorMessage if parsing fails
   */
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
