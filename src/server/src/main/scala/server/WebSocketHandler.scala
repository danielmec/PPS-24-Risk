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

object WebSocketHandler:
  case class IncomingMessage(text: String)
  case class OutgoingConnection(client: ActorRef)
  case object SendPing
  
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

  private class ConnectionActor(gameManager: ActorRef) extends Actor:
    var clientConnection: Option[ActorRef] = None
    
    private var pingScheduler: Option[akka.actor.Cancellable] = None
    
    override def preStart(): Unit = {
      println(s"[DEBUG] ConnectionActor ${self.path.name} avviato")
      
      import context.dispatcher
      pingScheduler = Some(context.system.scheduler.scheduleWithFixedDelay(
        50.seconds, 50.seconds, self, SendPing
      ))
    }
    
    override def postStop(): Unit = {
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
      case SendPing =>
        println(s"[DEBUG] Invio ping al client")
        sendProtocolMessage(protocol.ServerMessages.Ping())
      
      case OutgoingConnection(client) =>
        clientConnection = Some(client)
        
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
        
      case msg: protocol.ServerMessages.Error =>
       sendProtocolMessage(msg)
        
      case msg: ProtocolMessage =>
        val jsonString = messageToJson(msg).compactPrint
        clientConnection.foreach(_ ! TextMessage(jsonString))
        
      case _ => 
    }
    
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
    
    private def sendError(message: String): Unit =
      val errorMsg = protocol.ServerMessages.Error(message)
      sendProtocolMessage(errorMsg)
    
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
    
        case _ => 
          println(s"Messaggio non gestito: $msg")
          val errorMessage = protocol.ServerMessages.Error("UNSUPPORTED")
          sendProtocolMessage(errorMessage)
      }
    
    
    private def parseJsonMessage(text: String): Try[ProtocolMessage] = Try {
      text.parseJson.convertTo[ProtocolMessage]
    }
    

end WebSocketHandler