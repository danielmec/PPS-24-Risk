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



class ClientNetworkManager:
  
  implicit val system: ActorSystem[Nothing] = ActorSystem(Behaviors.empty, "risk-client")
  implicit val executionContext: ExecutionContext = system.executionContext
  
  
  private val serverHost = "localhost"
  private val serverPort = 8080
  private val baseUrl = s"http://$serverHost:$serverPort"
  private val wsUrl = s"ws://$serverHost:$serverPort/ws"
  
  
  private var playerId: Option[String] = None
  private var webSocketConnection: Option[(Future[WebSocketUpgradeResponse], Promise[Option[Message]])] = None
  private var outgoingMessages: Option[akka.actor.ActorRef] = None
  
  private var messageQueue: Option[SourceQueueWithComplete[String]] = None
  
  
  private var messageCallbacks: Map[String, Any => Unit] = Map.empty
  
  private var filteredCallbacks: Map[String, (Any, String) => Unit] = Map.empty

  
  private var lastGameState: Option[GameState] = None

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
   * Sends a message to the WebSocket server.
   * The message is queued and sent asynchronously.
   * Returns a Future indicating whether the message was successfully queued.
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
   * Used from client UI to send messages to the server.
   * It uses the method `sendWebSocketMessage` to send the message as a JSON string.
   **/
  def sendMessage[T](messageObject: T): Future[Boolean] = {
    val jsonString = ClientJsonSupport.toJson(messageObject).compactPrint
    sendWebSocketMessage(jsonString)
  }
  
  def getPlayerId: Option[String] = playerId

  def getLastGameState(): Option[GameState] = lastGameState

  def shutdown(): Unit =
    
    webSocketConnection.foreach { case (_, promise) => 
      promise.success(None) 
    }
    system.terminate()

  
  def registerCallback(messageType: String, callback: Any => Unit): Unit = {
    messageCallbacks = messageCallbacks + (messageType -> callback)
  }

  
end ClientNetworkManager
