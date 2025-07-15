package server

import akka.actor.Actor
import akka.actor.ActorSystem
import akka.actor.ActorRef
import akka.actor.Props
import akka.http.scaladsl.model.ws.TextMessage
import akka.http.scaladsl.model.ws.Message
import akka.stream.scaladsl.Sink
import akka.stream.scaladsl.Flow
import akka.stream.scaladsl.Source
import akka.stream.OverflowStrategy
import akka.NotUsed
import scala.util.Failure
import scala.util.Try
import scala.util.Success
import protocol.{Message => ProtocolMessage}
import spray.json._
import protocol.JsonSupport._
import scala.concurrent.duration._

/**
 * Handles WebSocket connections for the Risk game server.
 * Provides functionality for bidirectional communication between clients and the server.
 */
object WebSocketHandler:
  /**
   * Represents a message received from a client.
   * 
   * @param text The raw text content of the message
   */
  case class IncomingMessage(text: String)
  
  /**
   * Represents a connection to a client for sending messages.
   * 
   * @param client Reference to the client actor
   */
  case class OutgoingConnection(client: ActorRef)
  
  /**
   * Message sent periodically to trigger a ping to the client.
   */
  case object SendPing
  
  /**
   * Creates a Flow that handles WebSocket messages for a client connection.
   * Sets up bidirectional communication between clients and the server.
   * 
   * @param gameManager Reference to the game manager actor
   * @param system The actor system
   * @return Flow that processes WebSocket messages
   */
  def apply(gameManager: ActorRef)(implicit system: ActorSystem): Flow[Message, Message, NotUsed] =
    
    val connectionId = java.util.UUID.randomUUID().toString.take(8)
    val handler = system.actorOf(Props(new ConnectionActor(gameManager)), s"connection-$connectionId")
    val incoming = Flow[Message]
      .collect {
        case TextMessage.Strict(text) => IncomingMessage(text)
      }
      .to(Sink.actorRef(
        ref = handler,  
        onCompleteMessage = akka.actor.PoisonPill, 
        onFailureMessage = { case _ => akka.actor.PoisonPill } 
      ))
    val outgoing = Source.actorRef(
      PartialFunction.empty,  
      PartialFunction.empty,  
      16,                     
      OverflowStrategy.dropHead
    )
    Flow.fromSinkAndSourceMat(incoming, outgoing) { (_, outActor) =>
      handler ! OutgoingConnection(outActor)
      NotUsed
    }

  /**
   * Actor that handles a single WebSocket connection.
   * Processes messages to and from a connected client and communicates with the game manager.
   * 
   * @param gameManager Reference to the game manager actor
   */
  private class ConnectionActor(gameManager: ActorRef) extends Actor:
    var clientConnection: Option[ActorRef] = None
    private var pingScheduler: Option[akka.actor.Cancellable] = None
    
    /**
     * Initializes the connection actor.
     * Sets up a ping scheduler to keep the connection alive.
     */
    override def preStart(): Unit = {      
      import context.dispatcher
      pingScheduler = Some(context.system.scheduler.scheduleWithFixedDelay(
        50.seconds, 50.seconds, self, SendPing
      ))
    }
    
    /**
     * Cleans up resources when the connection actor is stopped.
     * Cancels the ping scheduler and notifies the game manager of disconnection.
     */
    override def postStop(): Unit = {
      pingScheduler.foreach(_.cancel())
      try {
        gameManager ! GameManager.PlayerDisconnected(self)
      } catch {
        case ex: Exception => 
          println(s"[ERROR] Errore nell'invio della notifica di disconnessione: ${ex.getMessage}")
          ex.printStackTrace()
      }
    }
    
    /**
     * Processes incoming and outgoing messages for this connection.
     * Handles protocol messages, connection setup, and client commands.
     * 
     * @return Receive function for handling messages
     */
    def receive = {
      case SendPing =>
        sendProtocolMessage(protocol.ServerMessages.Ping())
      
      case OutgoingConnection(client) => clientConnection = Some(client)
        
      case IncomingMessage(text) =>
        parseJsonMessage(text) match {
          case Success(msg) => 
            msg match {
              case protocol.ClientMessages.Pong() =>
              case other => 
                handleClientMessage(other)
            }
          case Failure(ex) => 
            println(s"[ERROR] Errore nel parsing del messaggio: ${ex.getMessage}")
        }
        
      case msg: protocol.ServerMessages.Error => sendProtocolMessage(msg)
        
      case msg: ProtocolMessage =>
        val jsonString = messageToJson(msg).compactPrint
        clientConnection.foreach(_ ! TextMessage(jsonString))
        
      case _ => 
    }
    
    /**
     * Sends a protocol message to the connected client.
     * Converts the message to JSON and sends it as a TextMessage.
     * 
     * @param msg The protocol message to send
     */
    private def sendProtocolMessage(msg: ProtocolMessage): Unit =
      try {
        val jsonValue = messageToJson(msg)
        val jsonString = jsonValue.compactPrint
        clientConnection.foreach { conn => 
          conn ! TextMessage(jsonString)
        }
      } catch {
        case ex: Exception =>
          println(s"[ERRORE] Impossibile inviare messaggio: ${ex.getMessage}")
          ex.printStackTrace()
      }
    
    /**
     * Sends an error message to the connected client.
     * 
     * @param message The error message text
     */
    private def sendError(message: String): Unit =
      val errorMsg = protocol.ServerMessages.Error(message)
      sendProtocolMessage(errorMsg)
    
    /**
     * Processes a client protocol message and routes it to the appropriate handler.
     * Forwards game-related messages to the game manager.
     * 
     * @param msg The client message to process
     */
    private def handleClientMessage(msg: ProtocolMessage): Unit =
      msg match {
        case protocol.ClientMessages.CreateGame(name, maxPlayers, username, numBots, botStrategies, botNames) =>
          println(s"Inoltro richiesta creazione partita: $name ($maxPlayers giocatori, $numBots bot)")
          gameManager ! GameManager.CreateGameSession(name, maxPlayers, self, username, numBots, botStrategies, botNames)
          
        case protocol.ClientMessages.JoinGame(gameId, username) if gameId.isEmpty && username.isEmpty =>
          println("Inoltro richiesta join lobby")        
          val lobbyMessage = protocol.ServerMessages.LobbyJoined("Benvenuto nella sala d'attesa!")
          sendProtocolMessage(lobbyMessage)         
          gameManager ! GameManager.RegisterClient(self)
          
        case protocol.ClientMessages.JoinGame(gameId,username) =>
          println(s"Inoltro richiesta join partita: $gameId")
          gameManager ! GameManager.JoinGameSession(gameId, self, username)

        case protocol.ClientMessages.GetAllGames() =>
          println(s"Inoltro richiesta lista partite")
          gameManager ! GameManager.GetAllGames(self)
          
        case action: protocol.ClientMessages.GameAction =>
          println(s"Inoltro azione di gioco: ${action.action} per partita ${action.gameId}")  
          val playerId = self.path.name 
          gameManager ! GameManager.ProcessGameAction(action.gameId, playerId, action)
    
        case protocol.ClientMessages.LeaveGame(gameId) =>
          println(s"Inoltro richiesta uscita dalla partita: $gameId")
          gameManager ! GameManager.LeaveGameSession(gameId, self)
          
        case _ => 
          println(s"Messaggio non gestito: $msg")
          val errorMessage = protocol.ServerMessages.Error("UNSUPPORTED")
          sendProtocolMessage(errorMessage)
      }
    
    /**
     * Parses a JSON string into a protocol message.
     * 
     * @param text The JSON string to parse
     * @return Success with the parsed message or Failure with an exception
     */
    private def parseJsonMessage(text: String): Try[ProtocolMessage] = Try {
      text.parseJson.convertTo[ProtocolMessage]
    }
    

end WebSocketHandler