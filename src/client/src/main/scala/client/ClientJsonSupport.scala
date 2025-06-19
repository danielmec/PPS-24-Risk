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
  
  
  case class GameActionMessage(
    gameId: String, 
    action: String, 
    parameters: Map[String, String]
  )

  // Definizione dei messaggi che il client può ricevere
  case class GameCreatedMessage(gameId: String, gameName: String, creatorId: String)
  case class PlayerJoinedMessage(gameId: String, playerId: String)
  case class ErrorMessage(message: String)
  case class LobbyJoinedMessage(message: String)
  case class GameJoinedMessage(gameId: String, players: List[String], gameName: String)
  case class LoginResponse(playerId: String, message: Option[String] = None)
  case class PingMessage() 
  case class GameListMessage(games: List[String])
  
  // Nuovi messaggi di gioco che il client può ricevere
  case class GameSetupStartedMessage(
    gameId: String, 
    message: String
  )
  
  case class PlayerLeftMessage(
    gameId: String,
    player: String
  )
  
  // Classe per rappresentare il game state complesso
  case class Territory(name: String, owner: String, troops: Int)
  case class PlayerState(playerId: String, cards: Int, bonusTroops: Int)
  
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
  
  case class GameActionResultMessage(
    success: Boolean,
    message: String
  )
  
  case class TurnChangedMessage(
    gameId: String,
    playerId: String,
    phase: String
  )
  
  case class TerritoryUpdateMessage(
    gameId: String,
    territoryName: String,
    owner: String,
    troops: Int
  )
  
  case class BattleResultMessage(
    gameId: String,
    attackerTerritory: String,
    defenderTerritory: String,
    attackerLosses: Int,
    defenderLosses: Int,
    newOwner: Option[String]
  )
  
  case class GameOverMessage(
    gameId: String,
    winnerId: String,
    winnerUsername: String
  )

  // Formati per i messaggi in uscita
  implicit val createGameFormat: RootJsonFormat[CreateGameMessage] = jsonFormat3(CreateGameMessage)
  implicit val joinGameFormat: RootJsonFormat[JoinGameMessage] = jsonFormat2(JoinGameMessage)
  implicit val joinLobbyFormat: RootJsonFormat[JoinLobbyMessage] = jsonFormat0(JoinLobbyMessage)
  implicit val leaveGameFormat: RootJsonFormat[LeaveGameMessage] = jsonFormat1(LeaveGameMessage)
  implicit val getAllGamesFormat: RootJsonFormat[GetAllGamesMessage] = jsonFormat0(GetAllGamesMessage) 
  implicit val pingFormat: RootJsonFormat[PingMessage] = jsonFormat0(PingMessage)
  
  // Nuovo formato per GameAction
  implicit val gameActionFormat: RootJsonFormat[GameActionMessage] = jsonFormat3(GameActionMessage)

  // Formati per i messaggi in entrata
  implicit val gameCreatedFormat: RootJsonFormat[GameCreatedMessage] = jsonFormat3(GameCreatedMessage)
  implicit val playerJoinedFormat: RootJsonFormat[PlayerJoinedMessage] = jsonFormat2(PlayerJoinedMessage)
  implicit val errorFormat: RootJsonFormat[ErrorMessage] = jsonFormat1(ErrorMessage)
  implicit val lobbyJoinedFormat: RootJsonFormat[LobbyJoinedMessage] = jsonFormat1(LobbyJoinedMessage)
  implicit val gameJoinedFormat: RootJsonFormat[GameJoinedMessage] = jsonFormat3(GameJoinedMessage)
  implicit val loginResponseFormat: RootJsonFormat[LoginResponse] = jsonFormat2(LoginResponse)
  implicit val pongFormat: RootJsonFormat[PongMessage] = jsonFormat0(PongMessage)
  implicit val gameListFormat: RootJsonFormat[GameListMessage] = jsonFormat1(GameListMessage)
  
  // Nuovi formati per i messaggi di gioco
  implicit val gameSetupStartedFormat: RootJsonFormat[GameSetupStartedMessage] = jsonFormat2(GameSetupStartedMessage)
  implicit val playerLeftFormat: RootJsonFormat[PlayerLeftMessage] = jsonFormat2(PlayerLeftMessage)
  implicit val gameStateDataFormat: RootJsonFormat[GameStateData] = jsonFormat4(GameStateData)
  implicit val gameStateFormat: RootJsonFormat[GameState] = jsonFormat4(GameState)
  implicit val gameStartedFormat: RootJsonFormat[GameStartedMessage] = jsonFormat3(GameStartedMessage)
  implicit val gameActionResultFormat: RootJsonFormat[GameActionResultMessage] = jsonFormat2(GameActionResultMessage)
  implicit val turnChangedFormat: RootJsonFormat[TurnChangedMessage] = jsonFormat3(TurnChangedMessage)
  implicit val territoryUpdateFormat: RootJsonFormat[TerritoryUpdateMessage] = jsonFormat4(TerritoryUpdateMessage)
  implicit val battleResultFormat: RootJsonFormat[BattleResultMessage] = jsonFormat6(BattleResultMessage)
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
    
    case msg: GetAllGamesMessage => 
      JsObject("type" -> JsString("getAllGames"))
    
    case _: PongMessage => 
      JsObject("type" -> JsString("pong"))
      
    // nuovo caso per serializzare le azioni di gioco
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
  
  //Metodo per deserializzare i messaggi in entrata
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
          
        // nuovi casi per i messaggi di gioco
        case Some(JsString("gameSetupStarted")) =>
          val gameId = fields.getOrElse("gameId", JsString("")).convertTo[String]
          val message = fields.getOrElse("message", JsString("")).convertTo[String]
          GameSetupStartedMessage(gameId, message)
          
        case Some(JsString("gameStarted")) =>
          val gameId = fields.getOrElse("gameId", JsString("")).convertTo[String]
          val currentPlayerId = fields.getOrElse("currentPlayerId", JsString("")).convertTo[String]
          
          // estrae lo stato iniziale
          val initialStateJson = fields.getOrElse("initialState", JsObject.empty).asJsObject
          val initialStateFields = initialStateJson.fields
          
          // gestisce sia il vecchio che il nuovo formato
          val gameState = if (initialStateFields.contains("state")) {
            val stateJson = initialStateFields.getOrElse("state", JsObject.empty).asJsObject
            val stateFields = stateJson.fields
            
            if (stateFields.contains("gameStateDto") && stateFields("gameStateDto") != JsNull) {
              val dto = stateFields("gameStateDto").asJsObject
              
              //Costruisce il GameState utilizzando le informazioni dal DTO
              GameState(
                gameId = initialStateFields.getOrElse("gameId", JsString(gameId)).convertTo[String],
                players = initialStateFields.getOrElse("players", JsArray()).convertTo[List[String]],
                currentPlayer = initialStateFields.getOrElse("currentPlayer", JsString(currentPlayerId)).convertTo[String],
                state = GameStateData(
                  currentPlayer = dto.fields.getOrElse("currentPlayer", JsString(currentPlayerId)).convertTo[String],
                  currentPhase = dto.fields.getOrElse("currentPhase", JsString("PlacingTroops")).convertTo[String],
                  territories = dto.fields.getOrElse("territories", JsArray()).convertTo[List[JsObject]].map(
                    _.fields.map { case (k, v) => k -> v.toString.replace("\"", "") }
                  ),
                  playerStates = dto.fields.getOrElse("playerStates", JsArray()).convertTo[List[JsObject]].map(
                    _.fields.map { case (k, v) => k -> v.toString.replace("\"", "") }
                  )
                )
              )
            } else {
              // Fallback: crea uno stato vuoto ma valido
              GameState(
                gameId = gameId,
                players = initialStateFields.getOrElse("players", JsArray()).convertTo[List[String]],
                currentPlayer = currentPlayerId,
                state = GameStateData(
                  currentPlayer = currentPlayerId,
                  currentPhase = "PlacingTroops",
                  territories = List(),
                  playerStates = List()
                )
              )
            }
          } else {
            // formato molto vecchio, fallback sicuro
            GameState(
              gameId = gameId,
              players = initialStateFields.getOrElse("players", JsArray()).convertTo[List[String]],
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

        case Some(JsString("gameState")) =>
          val gameId = fields.getOrElse("gameId", JsString("")).convertTo[String]
          val players = fields.getOrElse("players", JsArray()).convertTo[List[String]]
          val currentPlayer = fields.getOrElse("currentPlayer", JsString("")).convertTo[String]
          
          
          val stateJson = fields.getOrElse("state", JsObject.empty).asJsObject
          val stateFields = stateJson.fields
          
          // geestisce sia il vecchio che il nuovo formato
          val gameStateData = if (stateFields.contains("gameStateDto") && stateFields("gameStateDto") != JsNull) {
            val dto = stateFields("gameStateDto").asJsObject
            
            GameStateData(
              currentPlayer = dto.fields.getOrElse("currentPlayer", JsString(currentPlayer)).convertTo[String],
              currentPhase = dto.fields.getOrElse("currentPhase", JsString("PlacingTroops")).convertTo[String],
              territories = dto.fields.getOrElse("territories", JsArray()).convertTo[List[JsObject]].map(
                _.fields.map { case (k, v) => k -> v.toString.replace("\"", "") }
              ),
              playerStates = dto.fields.getOrElse("playerStates", JsArray()).convertTo[List[JsObject]].map(
                _.fields.map { case (k, v) => k -> v.toString.replace("\"", "") }
              )
            )
          } else {
            // Formato vecchio o fallback
            stateJson.convertTo[GameStateData]
          }
          
          GameState(gameId, players, currentPlayer, gameStateData)
          
        case Some(JsString("gameActionResult")) =>
          val success = fields.getOrElse("success", JsBoolean(false)).convertTo[Boolean]
          val message = fields.getOrElse("message", JsString("")).convertTo[String]
          GameActionResultMessage(success, message)
          
        case Some(JsString("turnChanged")) =>
          val gameId = fields.getOrElse("gameId", JsString("")).convertTo[String]
          val playerId = fields.getOrElse("playerId", JsString("")).convertTo[String]
          val phase = fields.getOrElse("phase", JsString("")).convertTo[String]
          TurnChangedMessage(gameId, playerId, phase)
          
        case Some(JsString("territoryUpdate")) =>
          val gameId = fields.getOrElse("gameId", JsString("")).convertTo[String]
          val territoryName = fields.getOrElse("territoryName", JsString("")).convertTo[String]
          val owner = fields.getOrElse("owner", JsString("")).convertTo[String]
          val troops = fields.getOrElse("troops", JsNumber(0)).convertTo[Int]
          TerritoryUpdateMessage(gameId, territoryName, owner, troops)
          
        case Some(JsString("battleResult")) =>
          val gameId = fields.getOrElse("gameId", JsString("")).convertTo[String]
          val attackerTerritory = fields.getOrElse("attackerTerritory", JsString("")).convertTo[String]
          val defenderTerritory = fields.getOrElse("defenderTerritory", JsString("")).convertTo[String]
          val attackerLosses = fields.getOrElse("attackerLosses", JsNumber(0)).convertTo[Int]
          val defenderLosses = fields.getOrElse("defenderLosses", JsNumber(0)).convertTo[Int]
          val newOwner = fields.get("newOwner") match
            case Some(JsNull) => None
            case Some(JsString(owner)) => Some(owner)
            case _ => None
          BattleResultMessage(gameId, attackerTerritory, defenderTerritory, attackerLosses, defenderLosses, newOwner)
          
        case Some(JsString("gameOver")) =>
          val gameId = fields.getOrElse("gameId", JsString("")).convertTo[String]
          val winnerId = fields.getOrElse("winnerId", JsString("")).convertTo[String]
          val winnerUsername = fields.getOrElse("winnerUsername", JsString("")).convertTo[String]
          GameOverMessage(gameId, winnerId, winnerUsername)
          
        case Some(JsString("playerLeft")) =>
          val gameId = fields.getOrElse("gameId", JsString("")).convertTo[String]
          val player = fields.getOrElse("player", JsString("")).convertTo[String]
          PlayerLeftMessage(gameId, player)

        
          
        case Some(JsString(unknownType)) => 
          ErrorMessage(s"Tipo di messaggio sconosciuto: $unknownType")
          
        case _ => 
          ErrorMessage("Messaggio non valido: campo 'type' mancante")
    catch
      case ex: Exception => 
        ErrorMessage(s"Errore durante il parsing JSON: ${ex.getMessage}")

end ClientJsonSupport
