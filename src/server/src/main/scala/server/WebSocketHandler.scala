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

/**
 * Gestisce la comunicazione WebSocket tra client e server
 */
object WebSocketHandler:
  // Messaggi interni
  case class IncomingMessage(text: String)
  case class OutgoingConnection(client: ActorRef)
  
  
    //Crea un flow di base o generica (Template)
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
    
    def receive = {
      // Registra il canale di output
      case OutgoingConnection(client) =>
        clientConnection = Some(client)
        
      // Gestisce messaggi in arrivo dal client
      case IncomingMessage(text) =>
        parseJsonMessage(text) match {
          case Success(msg) => handleClientMessage(msg)
          case Failure(ex) => sendError(s"Errore di parsing: ${ex.getMessage}")
        }
        
      // Gestisce messaggi dal server GameSession da inviare al client
      case msg: protocol.ServerMessages.Error =>
       sendProtocolMessage(msg)
        
      case msg: ProtocolMessage =>
        val jsonString = messageToJson(msg).toString()
        clientConnection.foreach(_ ! TextMessage(jsonString))
        
      case _ => // ignora altri messaggi per ora
    }
    
    // Metodo unificato per inviare messaggi di protocollo
    private def sendProtocolMessage(msg: ProtocolMessage): Unit =
      try {
        val jsonValue = messageToJson(msg)
        val jsonString = jsonValue.toString
        println(s"[DEBUG]  Invio messaggio: $jsonString")
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
          
        case _ => 
          println(s"Messaggio non gestito: $msg")
          // Usa un oggetto di protocollo per l'errore
          val errorMessage = protocol.ServerMessages.Error("UNSUPPORTED")
          sendProtocolMessage(errorMessage)
      }
    
    
    private def parseJsonMessage(text: String): Try[ProtocolMessage] = Try {
      
      text.parseJson.convertTo[ProtocolMessage]
    }
    
    // metodo chiamato quando l'attore viene fermato
    override def postStop(): Unit = {
      println(s"[DEBUG] ConnectionActor ${self.path.name} fermato, notifica GameManager")
      try {
        gameManager ! GameManager.PlayerDisconnected(self)
      } catch {
        case ex: Exception => 
          println(s"[ERROR] Errore nell'invio della notifica di disconnessione: ${ex.getMessage}")
          ex.printStackTrace()
      }
    }

end WebSocketHandler