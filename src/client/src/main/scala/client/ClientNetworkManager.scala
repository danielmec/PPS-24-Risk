package client

import akka.actor.typed.ActorSystem
import akka.actor.typed.scaladsl.Behaviors
import akka.http.scaladsl.Http
import akka.http.scaladsl.model._
import akka.http.scaladsl.model.ws._
import akka.stream.scaladsl.{Flow, Keep, Sink, Source}
import akka.http.scaladsl.unmarshalling.Unmarshal
import akka.NotUsed
import scala.concurrent.{ExecutionContext, Future, Promise}
import scala.util.{Success, Failure, Try}
import client.ClientJsonSupport._


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
          // Estrae il corpo della risposta come stringa , successivamente si creeà un Parser dedicato per JSON
          Unmarshal(response.entity).to[String].map { jsonString =>
            // Semplice estrazione dell'ID del giocatore
            val idPattern = """"playerId":\s*"([^"]+)"""".r
            idPattern.findFirstMatchIn(jsonString) match
              case Some(m) => 
                val id = m.group(1)
                playerId = Some(id)
                println(s"Login effettuato con successo. ID: $id")
                true
              case None =>
                println("Impossibile estrarre l'ID dalla risposta")
                false
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
    // Verifica che sia stato effettuato il login
    if (playerId.isEmpty) {
      println("Errore: È necessario effettuare il login prima di connettersi al WebSocket")
      return Future.successful(false)
    }
    
    // Crea il flow per gestire i messaggi WebSocket
    val messageFlow: Flow[Message, Message, Promise[Option[Message]]] = 
      Flow.fromSinkAndSourceMat(
        // Sink per i messaggi in arrivo dal server
        Sink.foreach[Message] { message =>
          message match {
            case TextMessage.Strict(text) =>
              println(s"Messaggio ricevuto: $text")
              
              // Usa ClientJsonSupport per processare il messaggio
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
              // Consuma lo stream per evitare perdite di memoria
              dataStream.runWith(Sink.ignore)
          }
        },
        // Source per i messaggi da inviare al server
        Source.maybe[Message]
      )(Keep.right)
    
    // Crea la richiesta WebSocket
    val request = WebSocketRequest(uri = wsUrl)
    
    // Effettua la connessione
    val (upgradeResponse, promise) = 
      Http().singleWebSocketRequest(request, messageFlow)
    
    // Salva la connessione per un uso successivo
    webSocketConnection = Some((upgradeResponse, promise))
    
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
    webSocketConnection match {
      case Some((_, promise)) =>
        // Crea un nuovo promise per la risposta
        val newPromise = Promise[Option[Message]]()
        
        // Completa il promise corrente con il messaggio
        promise.success(Some(TextMessage(message)))
        
        // Sostituisce il promise nella connessione
        webSocketConnection = webSocketConnection.map { case (resp, _) => (resp, newPromise) }
        
        Future.successful(true)
        
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
