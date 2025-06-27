package server

import akka.actor.{ Actor,  ActorRef,  Props,  ActorLogging}
import protocol.{ Message, ServerMessages, ClientMessages }
import java.util.UUID
import scala.collection.mutable.{  Map, ListBuffer }

import engine.{GameEngine, GameState, TurnPhase, GameAction, EngineState}
import model.player.{Player => CorePlayer, PlayerImpl => CorePlayerImpl}
import model.player.PlayerColor

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
    
    // Dto sta per Data Transfer Object usato per inviare lo stato del gioco ai client
    case class TerritoryDto(name: String, owner: String, troops: String)
    case class TerritoryCardDto(id: String, territoryName: String, cardType: String)
    case class MissionCardDto(id: String, description: String, targetType: String, targetValue: String)
    case class PlayerStateDto(
      playerId: String, 
      cards: String, 
      bonusTroops: String,
      territoryCards: List[TerritoryCardDto] = List(),
      missionCard: Option[MissionCardDto] = None
    )
    case class GameStateDto(
      gameId: String,
      currentPlayer: String,
      currentPhase: String,
      territories: List[TerritoryDto],
      playerStates: List[PlayerStateDto]
    )
    
    private case object StartGame extends Command

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
    maxPlayers: Int ) extends Actor with ActorLogging:

    import GameSession._

    private var gameEngine: Option[GameEngine] = None
    private var corePlayers: List[model.player.PlayerImpl] = List.empty

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
                    
                    val updatedPlayerData = playerData + (playerId -> Player(playerId, playerRef, username))
                    
                    println(s"Player $playerId ($username) joined game $gameId, current players: ${updatedPlayers.keys.mkString(", ")}")
                    
                    val playersList = updatedPlayerData.values.map(p => s"${p.username} (${p.id})").toList
                    val playerIds = updatedPlayers.keys.toList

                    val updatedState = gameState + 
                        ("playerUsernames" -> updatedPlayerData.map { case (id, player) => (id, player.username) }.toMap)

                    
                    updatedPlayers.values.foreach(player =>
                        //notifica actorRef con un messaggio GameJoined 
                        player ! ServerMessages.GameJoined(gameId, playersList, gameName)   
                    )

                    
                    val newPhase = (updatedPlayers.size >= maxPlayers, phase) match
                        case(true, WaitingForPlayers) =>
                            log.info(s"Game $gameId has reached max players, starting setup")
                          
                            initializeGameEngine(updatedPlayerData.values.toList)
                            
                            updatedPlayers.values.foreach(player =>
                                player ! ServerMessages.GameSetupStarted(gameId, "Game setup in progress...")
                            )
                            
                            // invia un messaggio a se stesso per avviare il gioco automaticamente
                            self ! StartGame
                            
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

        
        case StartGame if phase == Setup =>
            log.info(s"Game $gameId setup completed, starting game")
            
            // Avvia il gioco assegnando territori iniziali ecc.
            startGame()
            
            val engineState = gameEngine.get.getGameState
            val currentPlayerId = engineState.turnManager.currentPlayer.id
            
            //converte lo state del engine in un GameStateDto con oggetti tipizzati
            val clientState = convertGameStateToClient(engineState)

            // Converte in lista e aggiunge i nomi utente per UI
            val playersList = playerData.values.map(p => s"${p.username} (${p.id})").toList

            // Notifica tutti i giocatori che la partita è iniziata
            players.values.foreach(player =>
                player ! ServerMessages.GameStarted(
                    gameId, 
                    currentPlayerId, 
                    scala.collection.immutable.Map(
                        "gameId" -> gameId,
                        "players" -> playersList,
                        "currentPlayer" -> currentPlayerId,
                        "gameStateDto" -> clientState  
                    )
                )
            )
            
            val mutableState = scala.collection.mutable.Map[String, Any]("gameStateDto" -> clientState)
            context.become(running(players, playerData, Playing, Some(currentPlayerId), mutableState))

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
                            
                            context.parent ! GameManager.GameSessionEnded(gameId)
                            context.stop(self)

                        case false =>
                            
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
                                    case `playerId` => 
                                        val remainingPlayers = updatedPlayers.keys.toList
                                        remainingPlayers.headOption 
                                    case _ =>
                                        Some(cp)
                            )
                            
                            context.become(running(updatedPlayers, updatedPlayerData, newPhase, newCurrentPlayer, updatedState))

        
        case ProcessAction(playerId, action) =>
            (players.get(playerId), phase, gameEngine) match
                case (_, _, None) if phase == Playing =>
                    log.error("Game engine not initialized but game is in Playing phase")
                    sender() ! ServerMessages.Error("Game engine not initialized")
                
                case (None, _, _) =>
                    sender() ! ServerMessages.Error(s"Player $playerId is not in game $gameId")
                
                case (Some(_), p, _) if p != Playing =>
                    players(playerId) ! ServerMessages.GameActionResult(
                        false, 
                        s"Cannot perform action: game is in ${p} phase"
                    )
                
                case (Some(_), Playing, Some(engine)) =>
                    log.info(s"Processing action ${action.action} from player $playerId")
                    
                    try {
                      // Converte l'azione client in azione core
                      val coreAction = convertToGameAction(action, playerId)
                      
                      //processa l'azione con il motore di gioco e ne prende il nuovo stato
                      val updatedGameState = engine.processAction(coreAction)
                    
                      val nextPlayerId = updatedGameState.turnManager.currentPlayer.id
                      
                      val clientState = convertGameStateToClient(updatedGameState)
                      val playersList = playerData.values.map(p => s"${p.username} (${p.id})").toList

                      players(playerId) ! ServerMessages.GameActionResult(true, "Action processed")

                      // Aggiorna tutti i giocatori con il nuovo stato
                      players.values.foreach(player => 
                        player ! ServerMessages.GameState(
                          gameId, 
                          playersList,
                          nextPlayerId,
                          scala.collection.immutable.Map("gameStateDto" -> clientState)  
                        )
                      )
                      
    
                       val mutableState = scala.collection.mutable.Map[String, Any]("gameStateDto" -> clientState)
                       context.become(running(players, playerData, phase, Some(nextPlayerId), mutableState))
                      
                    } catch {
                        case ex: Exception =>
                            log.error(s"Error processing action: ${ex.getMessage}")
                            players(playerId) ! ServerMessages.GameActionResult(
                                false,
                                s"Error processing action: ${ex.getMessage}"
                            )
                    }
                
                case _ =>
                    log.warning(s"Unexpected state in ProcessAction: player=$playerId, phase=$phase")
        
        
        case GetStateRequest =>
            // Lista di player con username
            val playersList = playerData.values.map(p => s"${p.username} (${p.id})").toList
            
            sender() ! ServerMessages.GameState(
                gameId,
                playersList,
                currentPlayer.getOrElse(""),
                gameState.toMap
            )
            
        case msg =>
            log.warning(s"GameSession received unhandled message: $msg")    

  
    /**
     * Inizializza il motore di gioco con i dati dei giocatori
     */
    private def initializeGameEngine(players: List[Player]): Unit = {
      // converte i giocatori server in giocatori core
      val corePlayers = players.map { player =>
        CorePlayerImpl(
          player.id, 
          player.username, 
          generatePlayerColor(player.id),
          model.player.PlayerType.Human  
        )
      }
      
      // inizializza il motore di gioco engine
      try {
        log.info(s"Initializing game engine for game $gameId with ${players.size} players")
        gameEngine = Some(new GameEngine(corePlayers, gameId))
      } catch {
        case ex: Exception => 
          log.error(s"Error initializing game engine: ${ex.getMessage}")
          gameEngine = None
      }
    }
    
    /**
     * Avvia il gioco utilizzando GameEngine 
     */
    private def startGame(): Unit = {
      gameEngine.foreach { engine =>
        try {
          log.info(s"Starting game $gameId")
          
          // usa l'engine per inizializzare il gioco 
          val finalState = engine.setup()
          
          /* GameEngine produce GameState
              convertGameStateToClient converte in GameStateDto
              Il DTO viene inserito in una Map[String, Any]
              La mappa viene serializzata in JSON */

          val clientState = convertGameStateToClient(finalState)
          
          log.info(s"Game $gameId started with ${finalState.playerStates.size} players")
          log.info(s"Current player: ${finalState.turnManager.currentPlayer.name}")
          log.info(s"Current phase: ${finalState.turnManager.currentPhase}")
          log.info(s"Initial state has ${finalState.board.territories.size} territories")
          log.info(s"First few territories: ${
            finalState.board.territories.take(5).map(t => 
              s"${t.name} (owner=${t.owner.map(_.id).getOrElse("none")}, troops=${t.troops})"
            ).mkString(", ")
          }")
          log.info(s"Player states: ${
            finalState.playerStates.map(ps => 
              s"${ps.playerId} (bonus=${ps.bonusTroops}, cards=${ps.territoryCards.size}, objective=${ps.objectiveCard.map(_.description).getOrElse("none")})"
            ).mkString(", ")
          }")
          
        } catch {
          case ex: Exception =>
            log.error(s"Error starting game: ${ex.getMessage}")
            ex.printStackTrace() 
        }
      }
    }
    
    

    /**
     * Converte un'azione client in un'azione core
     */
    private def convertToGameAction(
        clientAction: ClientMessages.GameAction, 
        playerId: String
    ): engine.GameAction = {
      clientAction.action match {
        case "attack" => 
          val defenderId = clientAction.parameters.getOrElse("defenderId", "")
          engine.GameAction.Attack(
            playerId,
            defenderId,
            clientAction.parameters.getOrElse("fromTerritory", ""),
            clientAction.parameters.getOrElse("toTerritory", ""),
            clientAction.parameters.getOrElse("troops", "0").toInt
          )
          
        case "place_troops" =>
          engine.GameAction.PlaceTroops(
            playerId,
            clientAction.parameters.getOrElse("troops", "0").toInt,
            clientAction.parameters.getOrElse("territory", "")
          )
          
        case "end_turn" =>
          engine.GameAction.EndTurn

        case "end_setup" =>
          engine.GameAction.EndSetup  
          
        case "reinforce" =>
          engine.GameAction.Reinforce(
            playerId,
            clientAction.parameters.getOrElse("from", ""),
            clientAction.parameters.getOrElse("to", ""),
            clientAction.parameters.getOrElse("troops", "0").toInt
          )

        case unknown =>
          throw new IllegalArgumentException(s"Unknown action: $unknown")
      }
    }
    
    /**
     * Converte lo stato del gioco dal formato core al formato client
     */
    private def convertGameStateToClient(gameState: engine.GameState): GameStateDto = {
      GameStateDto(
        gameId = gameState.gameId,
        currentPlayer = gameState.turnManager.currentPlayer.id,
        currentPhase = gameState.turnManager.currentPhase match {
          case TurnPhase.SetupPhase => "SetupPhase"  
          case TurnPhase.MainPhase => "MainPhase"
        },  // Modificato per usare solo le due fasi
        territories = gameState.board.territories.map { territory =>
          TerritoryDto(
            name = territory.name,
            owner = territory.owner.map(_.id).getOrElse(""),
            troops = territory.troops.toString
          )
        }.toList,
        playerStates = gameState.playerStates.map { playerState =>
          PlayerStateDto(
            playerId = playerState.playerId,
            cards = playerState.territoryCards.size.toString,
            bonusTroops = playerState.bonusTroops.toString,
            territoryCards = playerState.territoryCards.map { card =>
          
              TerritoryCardDto(
                id = card.hashCode().toString, 
                territoryName = card.territory.name, 
                cardType = card.cardImg.toString 
              )
            }.toList,
            missionCard = playerState.objectiveCard.map { mission => 
              
              val (targetType, targetValue) = extractObjectiveDetails(mission)
              MissionCardDto(
                id = mission.hashCode().toString, 
                description = mission.description,
                targetType = targetType,
                targetValue = targetValue
              )
            }
          )
        }.toList
      )
    }

    /**
     * Estrae i dettagli dell'obiettivo in base al tipo
     */
    private def extractObjectiveDetails(objective: model.cards.ObjectiveCard): (String, String) = {
      objective match {
        case ct: model.cards.ObjectiveCard.ConquerTerritories => 
          ("TERRITORY_COUNT", s"${ct.num}")
          
        case cc: model.cards.ObjectiveCard.ConquerContinents => 
          ("CONTINENTS", cc.continents.map(_.name).mkString(","))
          
        case dp: model.cards.ObjectiveCard.DefeatPlayer => 
          ("ELIMINATE_PLAYER", dp.targetColor.toString)
          
        case _ => ("UNKNOWN", "")
      }
    }

    /**
     * Genera un colore giocatore casuale
     */
    private def generatePlayerColor(playerId: String): PlayerColor = {
      // Genera un colore basato sull'ID del giocatore
      val colors = PlayerColor.values
      val index = Math.abs(playerId.hashCode % colors.length)
      colors(index)
    }
  
end GameSession
