package client

import spray.json._
import DefaultJsonProtocol._

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
  case class GameActionMessage(gameId: String, action: String, parameters: Map[String, String])

  // Definizione dei messaggi che il client può ricevere
  case class GameCreatedMessage(gameId: String, gameName: String, creatorId: String)
  case class PlayerJoinedMessage(gameId: String, playerId: String)
  case class ErrorMessage(message: String)
  case class LobbyJoinedMessage(message: String)
  case class GameJoinedMessage(gameId: String, players: List[String], gameName: String)
  case class LoginResponse(playerId: String, message: Option[String] = None)
  case class PingMessage() 
  case class GameListMessage(games: List[String])
  
  // Messaggi di gioco che il client può ricevere
  case class GameSetupStartedMessage(gameId: String, message: String)
  case class PlayerLeftMessage(gameId: String, player: String)
  
  // Strutture dati di gioco
  case class Territory(name: String, owner: String, troops: Int)
  case class PlayerState(playerId: String, cards: Int, bonusTroops: Int)
  case class MissionCardDto(id: String, description: String, targetType: String, targetValue: String)
  
  case class GameStateData(
    currentPlayer: String,
    currentPhase: String,
    territories: List[Map[String, String]],
    playerStates: List[Map[String, String]]
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
  case class BattleResultMessage(
    gameId: String,
    attackerTerritory: String,
    defenderTerritory: String,
    attackerLosses: Int,
    defenderLosses: Int,
    newOwner: Option[String], 
    attackerDice: Seq[Int],
    defenderDice: Seq[Int]
  )
  case class GameOverMessage(gameId: String, winnerId: String, winnerUsername: String)

  // Helper methods per l'estrazione di valori dai campi JSON
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
        // Estrai direttamente la description
        val description = v.fields.getOrElse("description", JsString(""))
        k -> description.convertTo[String]
      case (k, JsString(s)) => k -> s
      case (k, JsNumber(n)) => k -> n.toString
      case (k, JsBoolean(b)) => k -> b.toString
      case (k, JsNull) => k -> ""
      case (k, v) => k -> v.toString.replace("\"", "")
    }

  // Formati JSON per messaggi semplici (usati da spray-json)
  implicit val createGameFormat: RootJsonFormat[CreateGameMessage] = jsonFormat3(CreateGameMessage)
  implicit val joinGameFormat: RootJsonFormat[JoinGameMessage] = jsonFormat2(JoinGameMessage)
  implicit val joinLobbyFormat: RootJsonFormat[JoinLobbyMessage] = jsonFormat0(JoinLobbyMessage)
  implicit val leaveGameFormat: RootJsonFormat[LeaveGameMessage] = jsonFormat1(LeaveGameMessage)
  implicit val getAllGamesFormat: RootJsonFormat[GetAllGamesMessage] = jsonFormat0(GetAllGamesMessage) 
  implicit val pingFormat: RootJsonFormat[PingMessage] = jsonFormat0(PingMessage)
  implicit val gameActionFormat: RootJsonFormat[GameActionMessage] = jsonFormat3(GameActionMessage)
  implicit val gameCreatedFormat: RootJsonFormat[GameCreatedMessage] = jsonFormat3(GameCreatedMessage)
  implicit val playerJoinedFormat: RootJsonFormat[PlayerJoinedMessage] = jsonFormat2(PlayerJoinedMessage)
  implicit val errorFormat: RootJsonFormat[ErrorMessage] = jsonFormat1(ErrorMessage)
  implicit val lobbyJoinedFormat: RootJsonFormat[LobbyJoinedMessage] = jsonFormat1(LobbyJoinedMessage)
  implicit val gameJoinedFormat: RootJsonFormat[GameJoinedMessage] = jsonFormat3(GameJoinedMessage)
  implicit val loginResponseFormat: RootJsonFormat[LoginResponse] = jsonFormat2(LoginResponse)
  implicit val pongFormat: RootJsonFormat[PongMessage] = jsonFormat0(PongMessage)
  implicit val gameListFormat: RootJsonFormat[GameListMessage] = jsonFormat1(GameListMessage)
  implicit val gameSetupStartedFormat: RootJsonFormat[GameSetupStartedMessage] = jsonFormat2(GameSetupStartedMessage)
  implicit val playerLeftFormat: RootJsonFormat[PlayerLeftMessage] = jsonFormat2(PlayerLeftMessage)
  implicit val missionCardDtoFormat: RootJsonFormat[MissionCardDto] = jsonFormat4(MissionCardDto)
  
  // Formato personalizzato per GameStateData (gestione speciale di missionCard)
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
        ))
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
        }
      )
    }
  }
  
  implicit val gameStateFormat: RootJsonFormat[GameState] = jsonFormat4(GameState)
  implicit val gameStartedFormat: RootJsonFormat[GameStartedMessage] = jsonFormat3(GameStartedMessage)
  implicit val gameActionResultFormat: RootJsonFormat[GameActionResultMessage] = jsonFormat2(GameActionResultMessage)
  implicit val turnChangedFormat: RootJsonFormat[TurnChangedMessage] = jsonFormat3(TurnChangedMessage)
  implicit val territoryUpdateFormat: RootJsonFormat[TerritoryUpdateMessage] = jsonFormat4(TerritoryUpdateMessage)
  implicit val battleResultFormat: RootJsonFormat[BattleResultMessage] = jsonFormat8(BattleResultMessage)
  implicit val gameOverFormat: RootJsonFormat[GameOverMessage] = jsonFormat3(GameOverMessage)

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
  
  private def extractIntSeq(fields: Map[String, JsValue], key: String): Seq[Int] =
  fields.get(key) match
    case Some(JsArray(elements)) => elements.collect {
      case JsNumber(n) => n.toInt
    }
    case _ => Seq.empty[Int]
  // è una mappa per gestire i messaggi in arrivo
  //ogni chiave è il tipo di messaggio e il valore è una funzione che
  //prende i campi del messaggio e restituisce l'istanza del messaggio
  // In questo modo si evita l'uso di classi case per ogni messaggio
  //e rende il codice più conciso e flessibile.
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
        extractString(fields, "gameName")
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
        // fallback
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
        // Formato vecchio o fallback
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
        extractInt(fields, "attackerLosses"),
        extractInt(fields, "defenderLosses"),
        extractOption(fields, "newOwner")(_.convertTo[String]),
        extractIntSeq(fields, "attackerDice"),
        extractIntSeq(fields, "defenderDice") 
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
    }
  )

  // Metodo per deserializzare i messaggi in entrata (approccio funzionale)
  def fromJson(json: String): Any = 
    try
      val jsValue = json.parseJson
      val fields = jsValue.asJsObject.fields
      
      fields.get("type") match
        case Some(JsString(msgType)) if messageHandlers.contains(msgType) =>
          messageHandlers(msgType)(fields)
        
        case Some(JsString(unknownType)) => 
          println(s"[DEBUG] Tipo di messaggio sconosciuto ricevuto: '$unknownType'")
          println(s"[DEBUG] Contenuto completo del messaggio: ${jsValue.prettyPrint}")
          println(s"[DEBUG] Campi disponibili: ${fields.keys.mkString(", ")}")
          
          // Se c'è un campo "message" o "error", stampalo
          fields.get("message").foreach(msg => println(s"[DEBUG] Campo message: ${msg}"))
          fields.get("error").foreach(err => println(s"[DEBUG] Campo error: ${err}"))
          
          // Stampa i tipi di messaggi che possiamo gestire
          println(s"[DEBUG] Tipi di messaggio supportati: ${messageHandlers.keys.mkString(", ")}")
          
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
