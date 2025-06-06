package client

import akka.actor.typed.ActorSystem
import akka.actor.typed.scaladsl.Behaviors
import akka.http.scaladsl.Http
import akka.http.scaladsl.model._
import akka.http.scaladsl.model.ws._
import akka.stream.scaladsl.{Flow, Keep, Sink, Source}
import akka.stream.{OverflowStrategy, KillSwitches}
import akka.http.scaladsl.unmarshalling.Unmarshal
import akka.NotUsed
import akka.actor.typed.ActorRef
import scala.concurrent.{ExecutionContext, Future, Promise}
import scala.util.{Success, Failure, Try}
import client.ClientJsonSupport._
import spray.json._
import akka.stream.scaladsl.SourceQueueWithComplete
import akka.stream.QueueOfferResult


/**
 * Gestore delle comunicazioni di rete per il client Risiko
 */
class ClientNetworkManager:
  // Inizializzazione dell'ActorSystem per Akka HTTP
  implicit val system: ActorSystem[Nothing] = ActorSystem(Behaviors.empty, "risk-client")
  implicit val executionContext: ExecutionContext = system.executionContext
  
  // Configurazione del server
  private val serverHost = "localhost"
  private val serverPort = 8080
  private val baseUrl = s"http://$serverHost:$serverPort"
  private val wsUrl = s"ws://$serverHost:$serverPort/ws"
  
  // Stato del client (immutabile, usando Option)
  private var playerId: Option[String] = None
  private var webSocketConnection: Option[(Future[WebSocketUpgradeResponse], Promise[Option[Message]])] = None
  private var outgoingMessages: Option[akka.actor.ActorRef] = None
  //coda per i messaggi in uscita
  private var messageQueue: Option[SourceQueueWithComplete[String]] = None
  
  def login(username: String): Future[Boolean] =
    val request = HttpRequest(
      method = HttpMethods.POST,
      uri = s"$baseUrl/api/login",
      entity = HttpEntity(ContentTypes.`application/json`, s"""{"username": "$username"}""")
    )
    
    // Invia la richiesta e gestisce la risposta
    Http().singleRequest(request).flatMap { response =>
      response.status match
        case StatusCodes.OK =>
          Unmarshal(response.entity).to[String].map { jsonString =>
            try {
              //parseJson metodo della libreria spray-json per convertire la stringa JSON in un oggetto 
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
   * Connette il client al server tramite WebSocket
   * 
   * @return Future[Boolean] che indica il successo della connessione
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
    
    // Sink per messaggi in arrivo , simile a un onMessage del WebSocket tradizionale
    val incomingSink = Sink.foreach[Message] { message =>
      message match {
        case TextMessage.Strict(text) =>
          println(s"Messaggio ricevuto: $text")
          
          //si usa ClientJsonSupport per processare il messaggio
          val parsedMessage = ClientJsonSupport.fromJson(text)
          parsedMessage match {
            case LobbyJoinedMessage(message) =>
              println(s"Sei entrato nella lobby! $message")
            
            case ErrorMessage(message) =>
              println(s" Errore: $message")
            
            case GameCreatedMessage(gameId, gameName, creatorId) =>
              println(s" Partita creata: '$gameName' (ID: $gameId)")
            
            case PlayerJoinedMessage(gameId, playerId) =>
              println(s" Il giocatore $playerId è entrato nella partita $gameId")
            
            case GameJoinedMessage(gameId, players) =>
              println(s" Sei entrato nella partita $gameId con ${players.size} giocatori")
              println(s"   Giocatori: ${players.mkString(", ")}")
            
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
          //consuma lo stream per evitare perdite di memoria
          dataStream.runWith(Sink.ignore)
      }
    }
    
    // Crea il flow WebSocket
    val webSocketFlow = Flow.fromSinkAndSource(incomingSink, outgoingSource)
    
    // Effettua la connessione
    val (upgradeResponse, _) = Http().singleWebSocketRequest(
      WebSocketRequest(uri = wsUrl), 
      webSocketFlow
    )
    
    // Gestisce la risposta
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
   * Invia un messaggio tramite WebSocket
   * 
   * @param message Il messaggio da inviare
   * @return Future[Boolean] che indica il successo dell'invio
   */
  def sendWebSocketMessage(message: String): Future[Boolean] = {
    messageQueue match {
      case Some(queue) => 
        //offri il messaggio alla coda
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
  
  def getPlayerId: Option[String] = playerId

  def shutdown(): Unit =
    // Chiudi la connessione WebSocket se presente
    webSocketConnection.foreach { case (_, promise) => 
      promise.success(None) // Completa con None per chiudere
    }
    system.terminate()

end ClientNetworkManager
