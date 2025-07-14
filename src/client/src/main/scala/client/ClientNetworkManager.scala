package client

import akka.actor.typed.ActorSystem
import akka.actor.typed.scaladsl.Behaviors
import akka.http.scaladsl.Http
import akka.http.scaladsl.model._
import akka.http.scaladsl.model.ws._
import akka.stream.scaladsl.Flow
import akka.stream.scaladsl.Keep
import akka.stream.scaladsl.Source
import akka.stream.scaladsl.Sink
import akka.stream.KillSwitches
import akka.stream.OverflowStrategy
import akka.http.scaladsl.unmarshalling.Unmarshal
import akka.NotUsed
import akka.actor.typed.ActorRef
import scala.concurrent.Promise
import scala.concurrent.ExecutionContext
import scala.concurrent.Future
import scala.util.Failure
import scala.util.Success
import scala.util.Try
import client.ClientJsonSupport._
import spray.json._
import akka.stream.scaladsl.SourceQueueWithComplete
import akka.stream.QueueOfferResult

/**
 * Manages network communication between the client and server in the Risk game.
 * 
 * This class handles:
 * - Initial login to the server
 * - WebSocket connection setup and maintenance
 * - Sending/receiving messages to/from the server
 * - Message type-based callback registration
 * - Game state tracking
 */
class ClientNetworkManager:
  
  /** Actor system for handling asynchronous operations */
  implicit val system: ActorSystem[Nothing] = ActorSystem(Behaviors.empty, "risk-client")
  
  /** Execution context for handling futures */
  implicit val executionContext: ExecutionContext = system.executionContext
  
  /** Server host address */
  private val serverHost = "localhost"
  
  /** Server port number */
  private val serverPort = 8080
  
  /** Base URL for HTTP requests */
  private val baseUrl = s"http://$serverHost:$serverPort"
  
  /** WebSocket URL */
  private val wsUrl = s"ws://$serverHost:$serverPort/ws"
  
  /** Player's unique identifier assigned after login */
  private var playerId: Option[String] = None
  
  /** WebSocket connection details */
  private var webSocketConnection: Option[(Future[WebSocketUpgradeResponse], Promise[Option[Message]])] = None
  
  /** Reference to outgoing messages actor */
  private var outgoingMessages: Option[akka.actor.ActorRef] = None
  
  /** Queue for sending messages through WebSocket */
  private var messageQueue: Option[SourceQueueWithComplete[String]] = None
  
  /** Map of registered message callbacks by message type */
  private var messageCallbacks: Map[String, Any => Unit] = Map.empty
  
  /** Map of filtered callbacks that depend on additional parameters */
  private var filteredCallbacks: Map[String, (Any, String) => Unit] = Map.empty

  /** Most recent game state received from server */
  private var lastGameState: Option[GameState] = None

  /**
   * Authenticates the client with the server.
   *
   * @param username The username to login with
   * @return Future containing a boolean indicating success (true) or failure (false)
   */
  def login(username: String): Future[Boolean] =
    val request = HttpRequest(
      method = HttpMethods.POST,
      uri = s"$baseUrl/api/login",
      entity = HttpEntity(ContentTypes.`application/json`, s"""{"username": "$username"}""")
    )
    
    Http().singleRequest(request).flatMap { response =>
      response.status match
        case StatusCodes.OK =>
          Unmarshal(response.entity).to[String].map { jsonString =>
            try {
              val loginResponse = jsonString.parseJson.convertTo[LoginResponse]
              playerId = Some(loginResponse.playerId)
              println(s"Login effettuato con successo. ID: ${loginResponse.playerId}")
              true
            } catch {
              case ex: Exception =>
                println(s"Errore durante il parsing della risposta login: ${ex.getMessage}")
                false
            }
          }
        case _ =>
          println(s"Errore di connessione: ${response.status}")
          Future.successful(false)
    }.recover {
      case ex: Exception =>
        println(s"Errore di rete: ${ex.getMessage}")
        false
    }

  /**
   * Establishes a WebSocket connection with the server.
   * Requires a successful login before calling.
   *
   * @return Future containing a boolean indicating connection success or failure
   */
  def connectWebSocket(): Future[Boolean] = {
    if (playerId.isEmpty) {
      println("Errore: È necessario effettuare il login prima di connettersi al WebSocket")
      return Future.successful(false)
    }
    
    val (queue, outgoingSource) = Source
      .queue[String](100, OverflowStrategy.dropHead)
      .map(text => TextMessage.Strict(text))
      .preMaterialize()
    
    messageQueue = Some(queue)
    
    val incomingSink = Sink.foreach[Message] { message =>
      message match {
        case TextMessage.Strict(text) =>
          println(s"Messaggio ricevuto: $text")
          
          val parsedMessage = ClientJsonSupport.fromJson(text)
          parsedMessage match {
            // Messaggi esistenti
            case PingMessage() =>
              println("[DEBUG] Ricevuto ping dal server, invio pong")
              sendPong()
              
            case LobbyJoinedMessage(message) =>
              println(s"Sei entrato nella lobby! $message")
              messageCallbacks.get("lobbyJoined").foreach(_(parsedMessage))

            case ErrorMessage(message) =>
              println(s" Errore nell ClientNetworkManager: $message")
              messageCallbacks.get("error").foreach(_(parsedMessage))

            case msg @ GameCreatedMessage(gameId, gameName, creatorId) =>
              println(s" Partita creata: '$gameName' (ID: $gameId)")
              messageCallbacks.get("gameCreated").foreach(_(msg))

            case msg @ GameJoinedMessage(gameId, players, gameName, playerColors) =>
              println(s" Sei entrato nella partita $gameId -- $gameName con ${players.size} giocatori")
              println(s"   Giocatori: ${players.mkString(", ")}")
              if (playerColors.isDefined) {
                println(s"   Colori assegnati: ${playerColors.get.size}")
              }
              messageCallbacks.get("gameJoined").foreach(_(msg))

            case msg @ GameListMessage(games) =>
              println(s"Lista partite disponibili")
              messageCallbacks.get("gameList").foreach(_(msg))
            
            case msg @ GameSetupStartedMessage(gameId, message) =>
              println(s"Setup partita iniziato: $message")
              messageCallbacks.get("gameSetupStarted").foreach(_(msg))
              
            case msg @ GameStartedMessage(gameId, currentPlayerId, initialState) =>
              println(s"Partita iniziata. Giocatore corrente: $currentPlayerId")
              messageCallbacks.get("gameStarted").foreach(_(msg))
              
            case msg: GameState =>
              println(s"Aggiornamento stato di gioco. Fase: ${msg.state.currentPhase}")
              lastGameState = Some(msg)
              messageCallbacks.get("gameState").foreach(_(msg))
              
            case msg @ GameActionResultMessage(success, message) =>
              if (success) {
                println(s"Azione completata: $message")
              } else {
                println(s"Azione fallita: $message")
              }
              messageCallbacks.get("gameActionResult").foreach(_(msg))
              
            case msg @ TurnChangedMessage(gameId, playerId, phase) =>
              println(s"Cambio turno: giocatore $playerId, fase $phase")
              messageCallbacks.get("turnChanged").foreach(_(msg))
              
            case msg @ TerritoryUpdateMessage(gameId, territoryName, owner, troops) =>
              println(s"Territorio $territoryName aggiornato: proprietario=$owner, truppe=$troops")
              messageCallbacks.get("territoryUpdate").foreach(_(msg))
              
            case msg @ BattleResultMessage(gameId, attackerTerritory, defenderTerritory, attackerDice, defenderDice, attackerLosses, defenderLosses, conquered) =>
              println(s"Risultato battaglia: $attackerTerritory vs $defenderTerritory (dadi: ${attackerDice.mkString(",")} vs ${defenderDice.mkString(",")})")
              println(s"Perdite: attaccante $attackerLosses, difensore $defenderLosses")
              if (conquered) {
                println(s"Territorio $defenderTerritory conquistato!")
              }
              messageCallbacks.get("battleResult").foreach(_(msg))
              
            case msg @ GameOverMessage(gameId, winnerId, winnerUsername) =>
              println(s"Partita terminata. Vincitore: $winnerUsername ($winnerId)")
              messageCallbacks.get("gameOver").foreach(_(msg))
              
            case msg @ PlayerLeftMessage(gameId, player) =>
              println(s"Giocatore $player ha lasciato la partita $gameId")
              messageCallbacks.get("playerLeft").foreach(_(msg))
            
            case msg @ TroopMovementMessage(gameId, fromTerritory, toTerritory, troops, playerId) =>
              println(s"Spostamento truppe: $playerId sposta $troops truppe da $fromTerritory a $toTerritory")
              messageCallbacks.get("troopMovement").foreach(_(msg))

            case _ =>
              println(s"Messaggio di tipo sconosciuto: $text")
          }
          
        case TextMessage.Streamed(textStream) =>
          textStream.runFold("")(_ + _).foreach { text =>
            println(s"Messaggio streamed ricevuto: $text")
          }
        case BinaryMessage.Strict(data) =>
          println(s"Messaggio binario ricevuto: ${data.length} bytes")
        case BinaryMessage.Streamed(dataStream) =>
          dataStream.runWith(Sink.ignore)
      }
    }
    
    val webSocketFlow = Flow.fromSinkAndSource(incomingSink, outgoingSource)
    
    val (upgradeResponse, _) = Http().singleWebSocketRequest(
      WebSocketRequest(uri = wsUrl), 
      webSocketFlow
    )
    
    upgradeResponse.map { upgrade =>
      if (upgrade.response.status == StatusCodes.SwitchingProtocols) {
        println("Connessione WebSocket stabilita")
        true
      } else {
        println(s"Connessione WebSocket fallita: ${upgrade.response.status}")
        false
      }
    }.recover {
      case ex: Exception =>
        println(s"Errore durante la connessione WebSocket: ${ex.getMessage}")
        false
    }
  }
  
  /**
   * Sends a raw string message to the WebSocket server.
   * The message is queued and sent asynchronously.
   *
   * @param message The string message to send
   * @return Future indicating whether the message was successfully queued
   **/
  def sendWebSocketMessage(message: String): Future[Boolean] = {
    messageQueue match {
      case Some(queue) => 
        queue.offer(message).map {
          case QueueOfferResult.Enqueued => 
            println("Messaggio accodato con successo")
            true
          case QueueOfferResult.Dropped => 
            println("Messaggio scartato: coda piena")
            false
          case QueueOfferResult.Failure(ex) => 
            println(s"Errore nell'invio: ${ex.getMessage}")
            false
          case QueueOfferResult.QueueClosed => 
            println("Coda chiusa")
            false
        }
      case None =>
        println("Errore: Nessuna connessione WebSocket attiva")
        Future.successful(false)
    }
  }

  /**
   * Sends a pong response to the server.
   * Used to keep the WebSocket connection alive.
   */
  def sendPong(): Unit = {
    val pongMessage = ClientJsonSupport.toJson(PongMessage()).compactPrint
    
    messageQueue.foreach { queue =>
      queue.offer(pongMessage).onComplete {
        case Success(QueueOfferResult.Enqueued) => 
          println("[DEBUG] Pong inviato al server")
        case Success(_) => 
          println("[WARNING] Impossibile inviare pong: coda non disponibile")
        case Failure(ex) => 
          println(s"[ERROR] Errore nell'invio del pong: ${ex.getMessage}")
      }
    }
  }
  
  /**
   * Sends a message object to the server after converting it to JSON.
   * Used by the client UI to send various message types to the server.
   *
   * @param messageObject The message object to send
   * @return Future indicating whether the message was successfully queued
   **/
  def sendMessage[T](messageObject: T): Future[Boolean] = {
    val jsonString = ClientJsonSupport.toJson(messageObject).compactPrint
    sendWebSocketMessage(jsonString)
  }
  
  /**
   * Returns the player ID assigned by the server after login.
   *
   * @return Option containing the player ID, or None if not logged in
   */
  def getPlayerId: Option[String] = playerId

  /**
   * Returns the most recent game state received from the server.
   *
   * @return Option containing the most recent GameState, or None if no state has been received
   */
  def getLastGameState(): Option[GameState] = lastGameState

  /**
   * Shuts down the network manager and releases all resources.
   * Terminates the actor system and closes WebSocket connections.
   */
  def shutdown(): Unit =
    webSocketConnection.foreach { case (_, promise) => 
      promise.success(None) 
    }
    system.terminate()

  /**
   * Registers a callback function to be invoked when a specific message type is received.
   *
   * @param messageType The type of message to register for (e.g., "gameState", "turnChanged")
   * @param callback The function to call when a message of the specified type is received
   */
  def registerCallback(messageType: String, callback: Any => Unit): Unit = {
    messageCallbacks = messageCallbacks + (messageType -> callback)
  }

  
end ClientNetworkManager
