package server

import akka.actor.{ActorSystem, ActorRef, Props, Actor}
import akka.http.scaladsl.model.ws.{Message, TextMessage}
import akka.stream.scaladsl.{Flow, Sink, Source}
import akka.stream.OverflowStrategy
import akka.NotUsed
import scala.util.{Success, Failure, Try}
import protocol.{Message => ProtocolMessage}

/**
 * Gestisce la comunicazione WebSocket tra client e server
 */
object WebSocketHandler:
  // Messaggi interni
  case class IncomingMessage(text: String)
  case class OutgoingConnection(client: ActorRef)
  
  
    //Creata quando il client si connette al WebSocket tramite route ws
  def apply(gameManager: ActorRef)(implicit system: ActorSystem): Flow[Message, Message, NotUsed] =

    // Crea un nuovo attore ConnectionActor per gestire questa connessione 
    val handler = system.actorOf(Props(new ConnectionActor(gameManager)))
    
    // Gestisce i messaggi in ingresso dal client
    val incoming = Flow[Message]
      .collect {
        case TextMessage.Strict(text) => IncomingMessage(text)
      }
      .to(Sink.actorRef(handler, akka.actor.Status.Success(())))
    
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
        parseMessage(text) match {
          case Success(msg) => handleClientMessage(msg)
          case Failure(ex) => sendError(s"Errore di parsing: ${ex.getMessage}")
        }
        
      // Gestisce messaggi dal server da inviare al client
      case msg: protocol.ServerMessages.Error =>
        sendJson(s"""{"type":"error","message":"${msg.message}"}""")
        
      case _ => // ignora altri messaggi per ora
    }
    
    // Invia un messaggio di errore al client
    private def sendError(message: String): Unit =
      sendJson(s"""{"type":"error","message":"$message"}""")
    
    // Invia JSON al client
    private def sendJson(json: String): Unit =
      clientConnection.foreach(_ ! TextMessage(json)) // ! send a TextMessage
    
    // Gestisce i messaggi del client
    private def handleClientMessage(msg: ProtocolMessage): Unit =
      msg match {
        case protocol.ClientMessages.CreateGame(name, maxPlayers) =>
          gameManager ! GameManager.CreateGameSession(name, maxPlayers, self)
          
        case protocol.ClientMessages.JoinGame(gameId) =>
          gameManager ! GameManager.JoinGameSession(gameId, self)
          
        case _ => // altri messaggi per ora ignorati
      }
    
    // Parser semplificato (solo per esempio)
    private def parseMessage(text: String): Try[ProtocolMessage] =
      if text.contains("createGame")
        then Try(protocol.ClientMessages.CreateGame("Test Game", 4))
        else Failure(new Exception("Messaggio non riconosciuto"))

end WebSocketHandler