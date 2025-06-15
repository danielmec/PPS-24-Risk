package server

import akka.actor.{ActorSystem, ActorRef, Props, Actor}
import akka.http.scaladsl.model.ws.{Message, TextMessage}
import akka.stream.scaladsl.{Flow, Sink, Source}
import akka.stream.OverflowStrategy
import akka.NotUsed
import scala.util.{Success, Failure, Try}
import protocol.{Message => ProtocolMessage}

import spray.json._
import protocol.JsonSupport._

import scala.concurrent.duration._

/**
 * Gestisce la comunicazione WebSocket tra client e server
 */
object WebSocketHandler:
  // Messaggi interni
  case class IncomingMessage(text: String)
  case class OutgoingConnection(client: ActorRef)
  case object SendPing
  
  // Crea un flow di base o generica (Template)
  def apply(gameManager: ActorRef)(implicit system: ActorSystem): Flow[Message, Message, NotUsed] =
    
    val connectionId = java.util.UUID.randomUUID().toString.take(8)
    
    val handler = system.actorOf(Props(new ConnectionActor(gameManager)), s"connection-$connectionId")
    
    // Gestisce i messaggi in ingresso dal client
    val incoming = Flow[Message]
      .collect {
        case TextMessage.Strict(text) => IncomingMessage(text)
      }
      .to(Sink.actorRef(
        ref = handler,  
        onCompleteMessage = akka.actor.PoisonPill, // chiama postStop() dell'attore
        onFailureMessage = { case _ => akka.actor.PoisonPill } 
      ))
    
    // Crea una sorgente per i messaggi in uscita verso il client
    val outgoing = Source.actorRef(
      PartialFunction.empty,  // completionMatcher
      PartialFunction.empty,  // failureMatcher
      16,                     // bufferSize
      OverflowStrategy.dropHead
    )
    
    // Collega il tutto
    Flow.fromSinkAndSourceMat(incoming, outgoing) { (_, outActor) =>
      handler ! OutgoingConnection(outActor)
      NotUsed
    }
  
  /**
   * Attore che gestisce una singola connessione WebSocket
   */
  private class ConnectionActor(gameManager: ActorRef) extends Actor:
    // Riferimento per inviare messaggi al client
    var clientConnection: Option[ActorRef] = None
    
    // Aggiungi un campo per lo scheduler
    private var pingScheduler: Option[akka.actor.Cancellable] = None
    
    override def preStart(): Unit = {
      println(s"[DEBUG] ConnectionActor ${self.path.name} avviato")
      
      // ping ogni 50 secondi 
      import context.dispatcher
      pingScheduler = Some(context.system.scheduler.scheduleWithFixedDelay(
        50.seconds, 50.seconds, self, SendPing
      ))
    }
    
    override def postStop(): Unit = {
      // Cancella lo scheduler quando l'attore viene fermato
      pingScheduler.foreach(_.cancel())
      
      println(s"[DEBUG] ConnectionActor ${self.path.name} fermato, notifica GameManager")
      try {
        gameManager ! GameManager.PlayerDisconnected(self)
      } catch {
        case ex: Exception => 
          println(s"[ERROR] Errore nell'invio della notifica di disconnessione: ${ex.getMessage}")
          ex.printStackTrace()
      }
    }
    
    def receive = {
      // Aggiungi questo al pattern matching
      case SendPing =>
        println(s"[DEBUG] Invio ping al client")
        sendProtocolMessage(protocol.ServerMessages.Ping())
      
      // Registra il canale di output
      case OutgoingConnection(client) =>
        clientConnection = Some(client)
        
      // Gestisce messaggi in arrivo dal client
      case IncomingMessage(text) =>
        parseJsonMessage(text) match {
          case Success(msg) => 
            msg match {
              case protocol.ClientMessages.Pong() =>
                println(s"[DEBUG] Ricevuto pong dal client")
              case other => 
                handleClientMessage(other)
            }
          case Failure(ex) => 
            println(s"[ERROR] Errore nel parsing del messaggio: ${ex.getMessage}")
        }
        
      // Gestisce messaggi dal server GameSession da inviare al client
      case msg: protocol.ServerMessages.Error =>
       sendProtocolMessage(msg)
        
      //messaggi da GameManager o GameSession gia arrivati protocollati secondo Message.scala
      case msg: ProtocolMessage =>
        val jsonString = messageToJson(msg).compactPrint
        clientConnection.foreach(_ ! TextMessage(jsonString))
        
      case _ => // ignora altri messaggi per ora
    }
    
    // Metodo unificato per inviare messaggi di protocollo
    private def sendProtocolMessage(msg: ProtocolMessage): Unit =
      try {
        val jsonValue = messageToJson(msg)
        val jsonString = jsonValue.compactPrint
        println(s"[DEBUG]  Invio messaggio al client: $jsonString")
        clientConnection.foreach { conn => 
          conn ! TextMessage(jsonString)
          println(s"[DEBUG] Messaggio inviato al client")
        }
      } catch {
        case ex: Exception =>
          println(s"[ERRORE] Impossibile inviare messaggio: ${ex.getMessage}")
          ex.printStackTrace()
      }
    
    // Invia un messaggio di errore al client (versione aggiornata)
    private def sendError(message: String): Unit =
      val errorMsg = protocol.ServerMessages.Error(message)
      sendProtocolMessage(errorMsg)
    
    // Gestisce i messaggi del client
    private def handleClientMessage(msg: ProtocolMessage): Unit =
      msg match {
        case protocol.ClientMessages.CreateGame(name, maxPlayers) =>
          println(s"Inoltro richiesta creazione partita: $name ($maxPlayers giocatori)")
          gameManager ! GameManager.CreateGameSession(name, maxPlayers, self)
          
        case protocol.ClientMessages.JoinGame(gameId) if gameId.isEmpty =>
          // Caso speciale: join alla lobby generale
          println("Inoltro richiesta join lobby")
         
          val lobbyMessage = protocol.ServerMessages.LobbyJoined("Benvenuto nella sala d'attesa!")
          sendProtocolMessage(lobbyMessage)
          
          // Registra il client nel GameManager per ricevere notifiche
          gameManager ! GameManager.RegisterClient(self)
          
        case protocol.ClientMessages.JoinGame(gameId) =>
          println(s"Inoltro richiesta join partita: $gameId")
          gameManager ! GameManager.JoinGameSession(gameId, self)

        case protocol.ClientMessages.GetAllGames() =>
          println(s"Inoltro richiesta lista partite")
          gameManager ! GameManager.GetAllGames(self)
          
        case _ => 
          println(s"Messaggio non gestito: $msg")
          // Usa un oggetto di protocollo per l'errore
          val errorMessage = protocol.ServerMessages.Error("UNSUPPORTED")
          sendProtocolMessage(errorMessage)
      }
    
    
    private def parseJsonMessage(text: String): Try[ProtocolMessage] = Try {
      
      text.parseJson.convertTo[ProtocolMessage]
    }
    

end WebSocketHandler