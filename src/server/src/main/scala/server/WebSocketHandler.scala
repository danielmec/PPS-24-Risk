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
  
  
    //Creata un flow di base o generica (Template)
  def apply(gameManager: ActorRef)(implicit system: ActorSystem): Flow[Message, Message, NotUsed] =

    // definisce come gestire le connessioni WebSocket
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
        
      // Gestisce messaggi dal server GameSession da inviare al client
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
          println(s"Inoltro richiesta creazione partita: $name ($maxPlayers giocatori)")
          gameManager ! GameManager.CreateGameSession(name, maxPlayers, self)
          
        case protocol.ClientMessages.JoinGame(gameId) if gameId.isEmpty =>
          // Caso speciale: join alla lobby generale
          println("Inoltro richiesta join lobby")
          sendJson(s"""{"type":"lobbyJoined","message":"Benvenuto nella lobby!"}""")
          // Registra il client nel GameManager per ricevere notifiche sulle partite disponibili
          gameManager ! GameManager.RegisterClient(self)
          
        case protocol.ClientMessages.JoinGame(gameId) =>
          println(s"Inoltro richiesta join partita: $gameId")
          gameManager ! GameManager.JoinGameSession(gameId, self)
          
        case _ => 
          println(s"Messaggio non gestito: $msg")
          sendError(s"Tipo di messaggio non supportato")
      }
    
    
    private def parseMessage(text: String): Try[ProtocolMessage] = Try {
      
      if text.contains("createGame") then
        // Estrazione dei parametri
        val namePattern = "\"gameName\"\\s*:\\s*\"([^\"]+)\"".r
        val playersPattern = "\"maxPlayers\"\\s*:\\s*(\\d+)".r
        
        val gameName = namePattern.findFirstMatchIn(text)
          .map(_.group(1))
          .getOrElse("Nuova Partita")
        
        val maxPlayers = playersPattern.findFirstMatchIn(text)
          .map(_.group(1).toInt)
          .getOrElse(4)
        
        protocol.ClientMessages.CreateGame(gameName, maxPlayers)
    
      // Per joinGame o JOIN_LOBBY
      else if text.contains("joinGame") || text.contains("JOIN_LOBBY") then
        // Se è JOIN_LOBBY o manca gameId, considera come accesso alla sala delle lobby
        if text.contains("JOIN_LOBBY") || !text.contains("gameId") then
          // Usa "" come ID speciale per indicare la lobby generale
          protocol.ClientMessages.JoinGame("")
        else
          // Estrai l'ID della partita
          val gameIdPattern = "\"gameId\"\\s*:\\s*\"([^\"]+)\"".r
          val gameId = gameIdPattern.findFirstMatchIn(text)
            .map(_.group(1))
            .getOrElse("")
        
          protocol.ClientMessages.JoinGame(gameId)
    
      else
        throw new Exception("Messaggio non riconosciuto")
    }

end WebSocketHandler