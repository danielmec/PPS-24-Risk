package server

import akka.actor.{
    Actor, 
    ActorRef, 
    Props, 
    ActorLogging
}
import protocol.{
    Message,
    ServerMessages,
    ClientMessages
}

import java.util.UUID
import scala.collection.mutable.{
    Map,
    ListBuffer
}


//Oggetto companion per GameSession che definisce i messaggi e il factory method
object GameSession:

    sealed trait Command

    case class JoinGame(playerId: String, playerRef: ActorRef, username: String) extends Command
    case class LeaveGame(playerId: String) extends Command
    case class ProcessAction(
        playerId: String, 
        action: ClientMessages.GameAction
        ) extends Command
    case object GetStateRequest extends Command

    // Classe che rappresenta un giocatore con username
    case class Player(id: String, ref: ActorRef, username: String)

    // Enumeration per rappresentare le fasi del gioco
    sealed trait GamePhase
    case object WaitingForPlayers extends GamePhase
    case object Playing extends GamePhase
    case object Finished extends GamePhase
    case object Setup extends GamePhase

    // Factory method per creare un'istanza di GameSession con 3 parametri 
    def props(gameId: String, gameName: String, maxPlayers: Int): Props = 
        Props(new GameSession(gameId, gameName, maxPlayers))


class GameSession(
    gameId: String, 
    gameName: String, 
    maxPlayers: Int
) extends Actor with ActorLogging:

    import GameSession._

    override def preStart(): Unit =
        log.info(s"GameSession $gameId started with name $gameName and max players $maxPlayers")
        context.become(running(
            players = Map.empty, 
            playerData = Map.empty,
            phase = WaitingForPlayers,
            currentPlayer = None,
            gameState = Map.empty
        ))

    def receive: Receive = 
     case _ => // Dummy case to initialize the actor
        log.info(s"GameSession received message before running state")

        
    // funzione pura che gestisce lo stato running
    // parametri che gestiscono lo stato interno di una sessione di Game 
    // come i giocatori, numero ,fase...
    def running(
        players: Map[String, ActorRef],
        playerData: Map[String, Player],  
        phase: GamePhase,
        currentPlayer: Option[String],
        gameState: Map[String, Any]
    ): Receive = 
        //chiamato da GameManager , playerId sarebbe creator.name che è ID dell attore akka
        case JoinGame(playerId, playerRef, username) =>
            (players.size < maxPlayers, phase) match
                case (true, _) | (_, WaitingForPlayers) =>
                    //Creo una nuova mappa immutabile
                    val updatedPlayers = players + (playerId -> playerRef)
                    // Aggiungi il player con username alla mappa playerData
                    val updatedPlayerData = playerData + (playerId -> Player(playerId, playerRef, username))
                    
                    println(s"Player $playerId ($username) joined game $gameId, current players: ${updatedPlayers.keys.mkString(", ")}")
                    
                    // Lista di username da inviare ai client
                    val playersList = updatedPlayerData.values.map(p => s"${p.username} (${p.id})").toList
                    val playerIds = updatedPlayers.keys.toList

                    // Aggiorniamo lo stato per includere gli username
                    val updatedState = gameState + 
                        ("playerUsernames" -> updatedPlayerData.map { case (id, player) => (id, player.username) }.toMap)

                    //Notifico tutti i giocatori
                    updatedPlayers.values.foreach(player =>
                        //notifica actorRef con un messaggio GameJoined 
                        player ! ServerMessages.GameJoined(gameId, playersList, gameName)   
                    )

                    //calcolo il nuovo stato usando pattern matching
                    val newPhase = (updatedPlayers.size >= 2, phase) match
                        case(true, WaitingForPlayers) =>
                            log.info(s"Game $gameId has enough players, changing to Setup phase")
                            Setup
                        case _ => phase

                    //transizione di stato funzionale
                    context.become(running(updatedPlayers, updatedPlayerData, newPhase, currentPlayer, updatedState))

                case _ =>
                    //errore sul pattern matching
                    val errorMsg = phase match
                        case WaitingForPlayers => "Game is full"
                        case _ => "Game is already started"
                    // ! è un operatore send( invia errore a playerRef riferimento a ActorRef)
                    playerRef ! ServerMessages.Error(errorMsg)

        //chiamato da GameManager 
        case LeaveGame(playerId) =>
            players.get(playerId) match
                case None =>
                    log.warning(s"Player $playerId tried to leave game $gameId but is not partecipating")

                case Some(_) =>
                    val updatedPlayers = players - playerId // toglie il player
                    val updatedPlayerData = playerData - playerId // rimuove anche i dati del player

                    updatedPlayers.isEmpty match
                        case true =>
                            log.info(s"No more players in game $gameId, stopping session")
                            // Notifica il GameManager che questa sessione sta terminando
                            context.parent ! GameManager.GameSessionEnded(gameId)
                            context.stop(self)

                        case false =>
                            // Aggiorna le liste di player con username
                            val playersList = updatedPlayerData.values.map(p => s"${p.username} (${p.id})").toList
                            val playerIds = updatedPlayers.keys.toList
                            
                            val updatedState = gameState + 
                                ("players" -> playerIds) + 
                                ("playerUsernames" -> updatedPlayerData.map { case (id, player) => (id, player.username) }.toMap)

                            val username = playerData.get(playerId).map(_.username).getOrElse(playerId)
                            updatedPlayers.values.foreach(player => 
                                player ! ServerMessages.PlayerLeft(gameId, s"$username ($playerId)")
                                )

                            updatedPlayers.values.foreach(player =>
                                player ! ServerMessages.GameState(
                                    gameId,
                                    playersList,
                                    currentPlayer.getOrElse(""),
                                    updatedState.toMap
                                 )
                                )

                            val newPhase = updatedPlayers.size match
                                case n if n < 2 => WaitingForPlayers
                                case _ => phase

                            val newCurrentPlayer = currentPlayer.flatMap( cp =>
                                cp match
                                    case `playerId` => // se il giocatore corrente è quello che è uscito
                                        val remainingPlayers = updatedPlayers.keys.toList
                                        remainingPlayers.headOption // passa al primo giocatore rimanente
                                    case _ =>
                                        Some(cp)
                            )
                            
                            context.become(running(updatedPlayers, updatedPlayerData, newPhase, newCurrentPlayer, updatedState))

        // Pattern matching per ProcessAction
        case ProcessAction(playerId, action) =>
            (players.get(playerId), phase, currentPlayer) match
                case (None, _, _) =>
                    sender() ! ServerMessages.Error(s"Player $playerId is not in game $gameId")
                
                case (Some(_), p, _) if p != Playing =>
                    players(playerId) ! ServerMessages.GameActionResult(
                        false, 
                        s"Cannot perform action: game is in ${p} phase"
                    )
                
                case (Some(_), _, Some(cp)) if cp != playerId =>
                    players(playerId) ! ServerMessages.GameActionResult(
                        false, 
                        "It's not your turn"
                    )
                
                case (Some(_), Playing, Some(_)) =>
                    // Elabora l'azione in modo funzionale
                    val username = playerData.get(playerId).map(_.username).getOrElse(playerId)
                    log.info(s"Player $playerId ($username) performed action ${action.action} in game $gameId")
                    
                    // Rispondi al giocatore
                    players(playerId) ! ServerMessages.GameActionResult(true, "Action processed")
                    
                    // Crea un NUOVO stato combinando il vecchio con i nuovi dati (immutabile)
                    val newGameState = gameState + 
                        ("lastAction" -> action.action) + 
                        ("lastPlayer" -> playerId) +
                        ("lastPlayerUsername" -> username)
                    
                    // Calcola il prossimo giocatore in modo funzionale
                    val playerIds = players.keys.toList
                    val nextPlayer = findNextPlayer(playerId, playerIds)
                    
                    // Lista di player con username
                    val playersList = playerData.values.map(p => s"${p.username} (${p.id})").toList
                    
                    // Aggiorna tutti i giocatori
                    players.values.foreach(player => 
                        player ! ServerMessages.GameState(
                            gameId, 
                            playersList,
                            nextPlayer.getOrElse(""),
                            newGameState.toMap
                        )
                    )
                    
                    // Transizione di stato funzionale
                    context.become(running(players, playerData, phase, nextPlayer, newGameState))
                
                case _ =>
                    log.warning(s"Unexpected state in ProcessAction: player=$playerId, phase=$phase, currentPlayer=$currentPlayer")

        // Pattern matching per GetStateRequest
        case GetStateRequest =>
            // Lista di player con username
            val playersList = playerData.values.map(p => s"${p.username} (${p.id})").toList
            
            sender() ! ServerMessages.GameState(
                gameId,
                playersList,
                currentPlayer.getOrElse(""),
                gameState.toMap
            )
            
        // Pattern matching catch-all
        case msg =>
            log.warning(s"GameSession received unhandled message: $msg")    

    
    private def findNextPlayer(currentPlayerId: String, playerIds: List[String]): Option[String] =
        playerIds match
            case Nil => None
            case _ => 
                val currentIndex = playerIds.indexOf(currentPlayerId)
                currentIndex match
                    case -1 => playerIds.headOption //non è presente
                    case _ => Some(playerIds((currentIndex + 1) % playerIds.size)) // calcola indice

end GameSession
