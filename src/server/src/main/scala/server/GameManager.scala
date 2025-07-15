package server

import akka.actor.Actor
import akka.actor.Props
import akka.stream.Materializer
import scala.sys.Prop

import java.util.UUID
import akka.actor.ActorRef
import akka.actor.ActorLogging
import protocol.Message
import protocol.ClientMessages
import protocol.ServerMessages
import akka.pattern.ask
import scala.concurrent.duration._
import akka.util.Timeout

/**
 * Companion object for GameManager that defines message types and factory methods.
 * Contains command types for managing game sessions and player interactions.
 */
object GameManager:

    /**
     * Base trait for all commands that can be sent to the GameManager.
     */
    sealed trait Command

    /**
     * Command to create a new game session.
     * 
     * @param gameName The name of the new game
     * @param maxPlayers Maximum number of players allowed in the game
     * @param creator ActorRef of the client that created the game
     * @param username Username of the creator
     * @param numBots Number of AI bots to add to the game
     * @param botStrategies Optional list of strategy names for the bots
     * @param botNames Optional list of names for the bots
     */
    case class CreateGameSession(
      gameName: String, 
      maxPlayers: Int, 
      creator: ActorRef, 
      username: String,
      numBots: Int = 0,
      botStrategies: Option[List[String]] = None,
      botNames: Option[List[String]] = None
    ) extends Command
    
    /**
     * Command to join an existing game session.
     * 
     * @param gameId ID of the game to join
     * @param player ActorRef of the client joining the game
     * @param username Username of the joining player
     */
    case class JoinGameSession(gameId: String, player: ActorRef, username: String) extends Command
    
    /**
     * Command to leave a game session.
     * 
     * @param gameId ID of the game to leave
     * @param player ActorRef of the client leaving the game
     */
    case class LeaveGameSession(gameId: String, player: ActorRef) extends Command
    
    /**
     * Command to retrieve all active games.
     * 
     * @param replyTo ActorRef to send the list of games to
     */
    case class GetAllGames(replyTo: ActorRef) extends Command
    
    /**
     * Command to forward a message to a specific game session.
     * 
     * @param gameId ID of the target game
     * @param message Message to forward
     */
    case class ForwardToGame(gameId: String, message: Message) extends Command
    
    /**
     * Command to notify that a player has disconnected.
     * 
     * @param player ActorRef of the disconnected client
     */
    case class PlayerDisconnected(player: ActorRef) extends Command
    
    /**
     * Command to register a new client with the GameManager.
     * 
     * @param client ActorRef of the client to register
     */
    case class RegisterClient(client: ActorRef) extends Command
    
    /**
     * Command to notify that a game session has ended.
     * 
     * @param gameId ID of the ended game
     */
    case class GameSessionEnded(gameId: String) extends Command 

    /**
     * Command to process a game action from a player.
     * 
     * @param gameId ID of the game
     * @param playerId ID of the player performing the action
     * @param action The game action to process
     */
    case class ProcessGameAction(gameId: String, playerId: String, action: ClientMessages.GameAction) extends Command

    /**
     * Creates Props for a GameManager actor.
     * 
     * @return Props for creating a GameManager actor
     */
    def props: Props = Props(new GameManager())

    /**
     * Empty message used for initialization.
     */
    private case object Empty

/**
 * Actor that manages game sessions in the Risk game server.
 * Handles creating, joining, and ending game sessions, as well as tracking connected clients
 * and routing messages between clients and game sessions.
 */
class GameManager extends Actor with ActorLogging:

    import GameManager._
   
    /**
     * Initializes the GameManager with empty state when started.
     * Transitions to the running state with empty collections.
     */
    override def preStart(): Unit =
        log.info("GameManager started")
        context.become(running(Map.empty, Set.empty, Map.empty, Map.empty)) 

    /**
     * Initial receive method, only handles the Empty message.
     * 
     * @return Receive function for handling messages
     */
    def receive: Receive =
        case Empty => log.info("GameManager initialized with empty state") 
    
    /**
     * Main receive method that handles all GameManager commands.
     * Maintains state for active games, connected clients, player-to-game mappings,
     * and game name mappings.
     *
     * @param games Map of game IDs to GameSession actor references
     * @param connectedClients Set of connected client actor references
     * @param playerToGame Map of player actor references to their current game ID
     * @param gameNameMap Map of game IDs to game names
     * @return Receive function for handling messages with the current state
     */
    def running(
        games: Map[String, ActorRef], 
        connectedClients: Set[ActorRef],
        playerToGame: Map[ActorRef, String],
        gameNameMap: Map[String, String]
    ): Receive = 

        case RegisterClient(client) =>
            val updatedClients = connectedClients + client
            println(s"Client ${client.path} registered. Current clients: ${connectedClients.map(_.path)}")
            println(s"Updated clients: ${updatedClients.map(_.path)}")
            println(s"Total connected clients: ${updatedClients.size}")
            log.warning(s"Client ${client.path} connected. Total connected clients: ${updatedClients.size}")
            context.become(running(games, updatedClients, playerToGame, gameNameMap))
            
        case CreateGameSession(gameName, maxPlayers, creator, username, numBots, botStrategies, botNames) =>
            val gameId = UUID.randomUUID().toString().take(6) 
            log.warning(s"Creating game session: $gameName with ID: $gameId for user ${username} with $numBots bots")
            log.info(s"Bot strategies: ${botStrategies.getOrElse(List.empty).mkString(", ")}")
            val gameSession = context.actorOf(
                GameSession.props(gameId, gameName, maxPlayers, numBots, botStrategies, botNames),
                s"GameSession-$gameId"
            )
            val updatedGames = games + (gameId -> gameSession)
            val updatedPlayerToGame = playerToGame + (creator -> gameId)
            creator ! ServerMessages.GameCreated(
                gameId,
                gameName,
                creator.path.name
            )
            gameSession ! GameSession.JoinGame(creator.path.name, creator, username, numBots, botStrategies, botNames)
            val updatedGameNameMap = gameNameMap + (gameId -> gameName) // Salva l'associazione gameId -> gameName
            context.become(running(updatedGames, connectedClients, updatedPlayerToGame, updatedGameNameMap))

        case JoinGameSession(gameId, player, username) =>
            games.get(gameId) match 
                case Some(gameSession) =>
                    log.warning(s"Player ${player.path.name} joining game session: $gameId")
                    gameSession ! GameSession.JoinGame(
                        player.path.name, 
                        player, 
                        username, 
                        0,  
                        Some(List.empty),  
                        Some(List.empty)   
                    )
                    val updatedPlayerToGame = playerToGame + (player -> gameId)
                    context.become(running(games, connectedClients, updatedPlayerToGame, gameNameMap))
                    
                case None =>
                    log.warning(s"Game session $gameId not found for player ${player.path.name}")
                    player ! ServerMessages.Error(s"Game session $gameId not found")
            
        case LeaveGameSession(gameId, player) =>
            games.get(gameId) match 
                case Some(gameSession) =>
                    log.info(s"Player ${player.path.name} leaving game session: $gameId")
                    gameSession ! GameSession.LeaveGame(player.path.name)
                    val updatedPlayerToGame = playerToGame - player
                    context.become(running(games, connectedClients, updatedPlayerToGame, gameNameMap))
                    
                case None => log.warning(s"Game session $gameId not found for player ${player.path.name}")
            
        case GetAllGames(replyTo) =>
            val gameInfoList = games.keys.toList.map { gameId =>
                (gameId, gameNameMap.getOrElse(gameId, "Partita senza nome"))
            }
            replyTo ! ServerMessages.GameList(gameInfoList)

        case ForwardToGame(gameId, message) =>
            games.get(gameId) match 
                case Some(gameSession) => gameSession forward message
                case None =>
                    log.warning(s"Forwarding failed: Game session $gameId not found")
                    sender() ! ServerMessages.Error(s"Game session $gameId not found")
            
        case PlayerDisconnected(player) =>
            val updatedClients = connectedClients - player
            log.warning(s"Client ${player.path.name} disconnected. Total connected clients: ${updatedClients.size}")    
            playerToGame.get(player) match 
                case Some(gameId) =>
                    log.info(s"Player ${player.path.name} disconnected from game session: $gameId")
                    games.get(gameId) match 
                        case Some(gameSession) =>                         
                            gameSession ! GameSession.LeaveGame(player.path.name)                         
                            implicit val timeout: Timeout = 3.seconds
                            implicit val ec = context.dispatcher                           
                            context.system.scheduler.scheduleOnce(200.milliseconds) {
                                (gameSession ? GameSession.GetStateRequest).foreach {
                                    case state: ServerMessages.GameState if state.players.isEmpty => self ! GameSessionEnded(gameId)
                                    case _ => 
                                }
                            }
                            
                        case None => log.warning(s"Game session $gameId not found for disconnected player ${player.path.name}")

                case None => log.warning(s"Player ${player.path.name} not found")             
                val updatedPlayerToGame = playerToGame - player
                context.become(running(games, updatedClients, updatedPlayerToGame, gameNameMap))
        
        case GameSessionEnded(gameId) =>
            log.info(s"Game session $gameId has ended, removing from active games")           
            val updatedGames = games - gameId           
            connectedClients.foreach { client =>
                client ! ServerMessages.GameRemoved(gameId)
            }
            val playersInGame = playerToGame.filter(_._2 == gameId).keys.toSet
            val updatedPlayerToGame = playerToGame -- playersInGame
            val updatedGameNameMap = gameNameMap - gameId     
            context.become(running(updatedGames, connectedClients, updatedPlayerToGame, updatedGameNameMap))

        case ProcessGameAction(gameId, playerId, action) =>
            games.get(gameId) match 
                case Some(gameSession) =>
                    log.info(s"Forwarding game action '${action.action}' from player $playerId to game $gameId")
                    gameSession ! GameSession.ProcessAction(playerId, action)
                case None =>
                    log.warning(s"Game action processing failed: Game session $gameId not found")
                    sender() ! ServerMessages.Error(s"Game session $gameId not found")
            
end GameManager



