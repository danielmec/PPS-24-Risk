package server

import akka.actor.ActorLogging
import akka.actor.Actor
import akka.actor.ActorRef
import akka.actor.Props
import protocol.ClientMessages
import protocol.Message
import protocol.ServerMessages
import java.util.UUID
import scala.collection.mutable.ListBuffer
import scala.collection.mutable.Map
import engine.EngineState
import engine.GameEngine 
import engine.GameState 
import engine.TurnPhase 
import engine.GameAction 
import engine.BattleResult
import engine.BattleRoundResult
import model.player.{Player => CorePlayer, PlayerImpl => CorePlayerImpl}
import model.player.PlayerColor
import scalafx.scene.input.KeyCode.O

object GameSession:

    sealed trait Command

    case class JoinGame(
      playerId: String, 
      playerRef: ActorRef, 
      username: String, 
      numBots: Int, 
      botStrategies: Option[List[String]], 
      botNames: Option[List[String]]
      ) extends Command
    case class LeaveGame(playerId: String) extends Command
    case class ProcessAction(
        playerId: String, 
        action: ClientMessages.GameAction
        ) extends Command
    case object GetStateRequest extends Command
    case class BotBattleNotification(
        from: String,
        to: String, 
        attackerId: String,
        defenderId: String,
        battleResult: BattleRoundResult
    ) extends Command
    
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
      playerStates: List[PlayerStateDto],
      playerStartedTurn: String  
    )
    
    private case object StartGame extends Command

    case class Player(id: String, ref: ActorRef, username: String)

    sealed trait GamePhase
    case object WaitingForPlayers extends GamePhase
    case object Playing extends GamePhase
    case object Finished extends GamePhase
    case object Setup extends GamePhase

    def props(
        gameId: String, 
        gameName: String, 
        maxPlayers: Int, 
        numBots: Int = 0, 
        botStrategies: Option[List[String]] = None, 
        botNames: Option[List[String]] = None
    ): Props = 
        Props(new GameSession(gameId, gameName, maxPlayers, numBots, botStrategies, botNames))


class GameSession(
    gameId: String, 
    gameName: String, 
    maxPlayers: Int,
    numBots: Int = 0, 
    botStrategies: Option[List[String]] = None, 
    botNames: Option[List[String]] = None ) extends Actor with ActorLogging:

    import GameSession._

    private var gameEngine: Option[GameEngine] = None
    private var corePlayers: List[model.player.PlayerImpl] = List.empty

    private val playerColors = scala.collection.mutable.Map[String, PlayerColor]()

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
     case _ => 
        log.info(s"GameSession received message before running state")

    def running(
        players: Map[String, ActorRef],
        playerData: Map[String, Player],  
        phase: GamePhase,
        currentPlayer: Option[String],
        gameState: Map[String, Any]
    ): Receive = 
        case JoinGame(playerId, playerRef, username, _, _, _) => // ignora i parametri dei bot del messaggio
            (players.size < maxPlayers, phase) match
                case (true, _) | (_, WaitingForPlayers) =>
                    // separazione dei giocatori umani dai bot che iniziano con "bot-"
                    val (humanPlayers, botPlayers) = players.partition(!_._1.startsWith("bot-"))
                    val (humanPlayerData, botPlayerData) = playerData.partition(!_._1.startsWith("bot-"))
                    
                    // aggiunge il nuovo giocatore umano
                    val updatedHumanPlayers = humanPlayers + (playerId -> playerRef)
                    val updatedHumanPlayerData = humanPlayerData + (playerId -> Player(playerId, playerRef, username))
                    
                    //gestisce i bot (crea o mantiene)
                    val (finalBotPlayers, finalBotPlayerData) = manageBots(botPlayers, botPlayerData)
                    
                    //combina umani e bot nelle mappe finali
                    val finalPlayers = updatedHumanPlayers ++ finalBotPlayers
                    val finalPlayerData = updatedHumanPlayerData ++ finalBotPlayerData
                    
                    val playersList = finalPlayerData.values.map(p => s"${p.username} (${p.id})").toList
                    val playerIds = finalPlayers.keys.toList

                    // mappa di colori playerID -> colorName (inclusi i bot)
                    val playerColorMap = finalPlayerData.keys.map { id =>
                      val color = generatePlayerColor(id)
                      id -> color.toString
                    }.toMap
                    
                    println("\n=== MAPPATURA UTENTI-COLORI (inclusi bot) ===")
                    finalPlayerData.foreach { case (id, player) =>
                      val color = playerColorMap.getOrElse(id, "NON ASSEGNATO")
                      val botFlag = if (id.startsWith("bot-")) "[BOT] " else ""
                      println(s"${botFlag}${player.username} (ID: $id) -> Colore: $color")
                    }
                    
                    val updatedState = gameState + 
                        ("playerUsernames" -> finalPlayerData.map { case (id, player) => (id, player.username) }.toMap) +
                        ("playerColors" -> playerColorMap)
        
                    //invia la notifica con la lista completa (umani + bot)
                    updatedHumanPlayers.values.foreach(player =>
                        player ! ServerMessages.GameJoined(gameId, playersList, gameName, Some(playerColorMap))   
                    )
    
                    val newPhase = (finalPlayers.size >= maxPlayers, phase) match
                        case(true, WaitingForPlayers) =>
                            log.info(s"Game $gameId has reached max players, starting setup")
                          
                            initializeGameEngine(finalPlayerData.values.toList)
                            
                            // invia solo ai giocatori umani
                            updatedHumanPlayers.values.foreach(player =>
                                player ! ServerMessages.GameSetupStarted(gameId, "Game setup in progress...")
                            )
                            
                            self ! StartGame
                            
                            Setup
                            
                        case _ => phase
                        
                    context.become(running(finalPlayers, finalPlayerData, newPhase, currentPlayer, updatedState))

                case _ =>
                    val errorMsg = phase match
                        case WaitingForPlayers => "Game is full"
                        case _ => "Game is already started"
                    playerRef ! ServerMessages.Error(errorMsg)

        
        case StartGame if phase == Setup =>
            log.info(s"Game $gameId setup completed, starting game")
            
            startGame()
            
            val engineState = gameEngine.get.getGameState
            val currentPlayerId = engineState.turnManager.currentPlayer.id
            val clientState = convertGameStateToClient(engineState)
            val playersList = playerData.values.map(p => s"${p.username} (${p.id})").toList

            val humanPlayerActors = players.filter(entry => !entry._1.startsWith("bot-"))
            humanPlayerActors.values.foreach(player =>
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
            
            val isFirstPlayerBot = engineState.turnManager.currentPlayer.playerType == model.player.PlayerType.Bot
            if (isFirstPlayerBot) {
                log.info(s"Il primo giocatore $currentPlayerId è un bot, eseguo il suo turno automaticamente")
                try {
                    // Invece di eseguire solo un turno, esegui tutti i turni bot in sequenza
                    val humanPlayers = players.filter(entry => !entry._1.startsWith("bot-"))
                    val (finalBotState, finalNextPlayer) = executeBotTurnsUntilHuman(
                        gameEngine.get, 
                        currentPlayerId, 
                        humanPlayers, 
                        playersList
                    )
                    val finalClientState = convertGameStateToClient(finalBotState)

                    humanPlayerActors.values.foreach(player => 
                        player ! ServerMessages.GameState(
                            gameId, 
                            playersList,
                            finalNextPlayer,
                            scala.collection.immutable.Map("gameStateDto" -> finalClientState)  
                        )
                    )
                    
                    val mutableState = scala.collection.mutable.Map[String, Any]("gameStateDto" -> finalClientState)
                    context.become(running(players, playerData, Playing, Some(finalNextPlayer), mutableState))
                } catch {
                    case e: exceptions.GameOverException => 
                        log.info(s"Game $gameId is over during first bot turn! Winner: ${e.getMessage}")
                        
                        val winnerRegex = ".*Player (.*?) \\((.*?)\\) has won the game!.*".r
                        val winnerInfo = e.getMessage match {
                            case winnerRegex(name, color) => 
                                println(s"Estratto dal messaggio - Nome: $name, Colore: $color")
                                (name, color)
                            case _ => 
                                println(s"Pattern non corrispondente. Messaggio: ${e.getMessage}")
                                ("Unknown", "Unknown")
                        }
                        
                        val winnerId = playerData.find(_._2.username.toLowerCase == winnerInfo._1.toLowerCase).map(_._1).getOrElse("")
                        val winnerUsername = playerData.get(winnerId).map(_.username).getOrElse(winnerInfo._1)
                        
                        humanPlayerActors.values.foreach { player =>
                            player ! ServerMessages.GameOver(
                                gameId,
                                winnerId,
                                winnerUsername
                            )
                        }
                        
                        context.become(running(players, playerData, Finished, Some(winnerId), gameState))
                        context.parent ! GameManager.GameSessionEnded(gameId)
                    
                    case ex: Exception =>
                        log.error(s"Errore durante l'esecuzione del primo turno del bot: ${ex.getMessage}")
                        ex.printStackTrace()
                }
            } else {
                val mutableState = scala.collection.mutable.Map[String, Any]("gameStateDto" -> clientState)
                context.become(running(players, playerData, Playing, Some(currentPlayerId), mutableState))
            }

        case LeaveGame(playerId) =>
            players.get(playerId) match
                case None =>
                    log.warning(s"Player $playerId tried to leave game $gameId but is not partecipating")

                case Some(_) =>
                    val updatedPlayers = players - playerId 
                    val updatedPlayerData = playerData - playerId 

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
                            
                            val humanPlayers = updatedPlayers.filter(entry => !entry._1.startsWith("bot-"))
                            humanPlayers.values.foreach(player => 
                                player ! ServerMessages.PlayerLeft(gameId, s"$username ($playerId)")
                            )
                            println(s"=== Giocatore $username ($playerId) ha lasciato la partita $gameId ===")
                            humanPlayers.values.foreach(player =>
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
                      val coreAction = convertToGameAction(action, playerId)
                      println(s"=== ESECUZIONE AZIONE ===")
                      println(s"Tipo azione: ${coreAction.getClass.getSimpleName}")
                      println(s"Giocatore: $playerId")
                      println(s"Phase: ${engine.getGameState.turnManager.currentPhase}")
                      val updatedGameState = engine.processAction(coreAction)

                      
                      if (action.action == "trade_cards") {
                        val playerStateOpt = updatedGameState.playerStates.find(_.playerId == playerId)
                        val bonus = playerStateOpt.map(_.bonusTroops).getOrElse(0)
                        players(playerId) ! ServerMessages.TrisPlayed(gameId, playerId, bonus)
                      }
                      

                      val nextPlayerId = updatedGameState.turnManager.currentPlayer.id                     
                      println(s"[processAction] playerStartedTurn prima della conversione: ${updatedGameState.playerStartedTurn}")          
                      val clientState = convertGameStateToClient(updatedGameState)
                      val playersList = playerData.values.map(p => s"${p.username} (${p.id})").toList
                      players(playerId) ! ServerMessages.GameActionResult(true, "Action processed")
                      
                      if (action.action == "attack") {
                        val from = action.parameters.getOrElse("fromTerritory", "")
                        val to = action.parameters.getOrElse("toTerritory", "")
                        val attackerDice = getLastBattleAttackerDice(updatedGameState)
                        val defenderDice = getLastBattleDefenderDice(updatedGameState)
                        val attackerLosses = getLastBattleAttackerLosses(updatedGameState) 
                        val defenderLosses = getLastBattleDefenderLosses(updatedGameState)
                        val conquered = isLastBattleConquered(updatedGameState, to, playerId)

                        val humanPlayers = players.filter(entry => !entry._1.startsWith("bot-"))
                        humanPlayers.values.foreach(player => 
                          player ! ServerMessages.BattleResult(
                            gameId,
                            from,
                            to,
                            attackerDice,
                            defenderDice,
                            attackerLosses,
                            defenderLosses,
                            conquered
                          )
                        )
                      }

                      if (action.action == "reinforce") {
                        val from = action.parameters.getOrElse("from", "")
                        val to = action.parameters.getOrElse("to", "")
                        val troops = action.parameters.getOrElse("troops", "0").toInt
                        
                        val humanPlayers = players.filter(entry => !entry._1.startsWith("bot-"))
                        humanPlayers.values.foreach(player => 
                          player ! ServerMessages.TroopMovement(
                            gameId,
                            from,
                            to,
                            troops,
                            playerId
                          )
                        )
                      }

                      println(s"[sendGameState] playerStartedTurn nel clientState: ${clientState.playerStartedTurn}")
                      
                      val humanPlayers = players.filter(entry => !entry._1.startsWith("bot-"))
                      humanPlayers.values.foreach(player => 
                        player ! ServerMessages.GameState(
                          gameId, 
                          playersList,
                          nextPlayerId,
                          scala.collection.immutable.Map("gameStateDto" -> clientState)  
                        )
                      )
                      
                      val isNextPlayerBot = updatedGameState.turnManager.currentPlayer.playerType == model.player.PlayerType.Bot
                      if (isNextPlayerBot) {
                        log.info(s"Il prossimo giocatore $nextPlayerId è un bot, eseguo il suo turno automaticamente")
                        
                        //catena di turni bot
                        val humanPlayers = players.filter(entry => !entry._1.startsWith("bot-"))
                        val playersList = playerData.values.map(p => s"${p.username} (${p.id})").toList
                        val (finalBotState, finalNextPlayer) = executeBotTurnsUntilHuman(engine, nextPlayerId, humanPlayers, playersList)
                        val finalClientState = convertGameStateToClient(finalBotState)
                        
                        // alla fine invia lo stato finale dopo tutti i turni bot
                        humanPlayers.values.foreach(player => 
                          player ! ServerMessages.GameState(
                            gameId, 
                            playersList,
                            finalNextPlayer,
                            scala.collection.immutable.Map("gameStateDto" -> finalClientState)  
                          )
                        )
                        
                        val mutableState = scala.collection.mutable.Map[String, Any]("gameStateDto" -> finalClientState)
                        context.become(running(players, playerData, phase, Some(finalNextPlayer), mutableState))
                      } else {
                        val mutableState = scala.collection.mutable.Map[String, Any]("gameStateDto" -> clientState)
                        context.become(running(players, playerData, phase, Some(nextPlayerId), mutableState))
                      }
                    } catch 
                        case e: exceptions.GameOverException =>
                            log.info(s"Game $gameId is over! Winner: ${e.getMessage}")
                            
                            //ricerca il nome e il colore del vincitore dal messaggio dell'eccezione che arriva dal GameEngine
                            val winnerRegex = ".*Player (.*?) \\((.*?)\\) has won the game!.*".r
                            val winnerInfo = e.getMessage match {
                                case winnerRegex(name, color) => 
                                    println(s"Estratto dal messaggio - Nome: $name, Colore: $color")
                                    (name, color)
                                case _ => 
                                    println(s"Pattern non corrispondente. Messaggio: ${e.getMessage}")
                                    ("Unknown", "Unknown")
                            }
                            
                            val winnerId = playerData.find(_._2.username.toLowerCase == winnerInfo._1.toLowerCase).map(_._1).getOrElse("")
                            val winnerUsername = playerData.get(winnerId).map(_.username).getOrElse(winnerInfo._1)
                            
                            println(s"WinnerID trovato: $winnerId")
                            println(s"WinnerUsername trovato: $winnerUsername")
                            
                            val humanPlayers = players.filter(entry => !entry._1.startsWith("bot-"))
                            humanPlayers.values.foreach { player =>
                                player ! ServerMessages.GameOver(
                                    gameId,
                                    winnerId,
                                    winnerUsername
                                )
                            }
                            
                            context.become(running(players, playerData, Finished, Some(winnerId), gameState))
                            context.parent ! GameManager.GameSessionEnded(gameId)
                        
                        case ex: Exception =>
                            log.error(s"Error processing action: ${ex.getMessage}")
                            println(s"=== ERRORE DETTAGLIATO ===")
                            println(s"Tipo eccezione: ${ex.getClass.getName}")
                            println(s"Messaggio: ${ex.getMessage}")
                            println(s"Stack trace:")
                            ex.printStackTrace()
                            println(s"=========================")
                            
                            players(playerId) ! ServerMessages.GameActionResult(
                                false,
                                s"Error processing action: ${ex.getMessage}"
                            )
                
                case _ =>
                    log.warning(s"Unexpected state in ProcessAction: player=$playerId, phase=$phase")
        
        
        case GetStateRequest =>
            val playersList = playerData.values.map(p => s"${p.username} (${p.id})").toList
            
            sender() ! ServerMessages.GameState(
                gameId,
                playersList,
                currentPlayer.getOrElse(""),
                gameState.toMap
            )
            
        case BotBattleNotification(from, to, attackerId, defenderId, battleResult) =>
            log.info(s"Gestione notifica battaglia bot: $attackerId ha eseguito un attacco: $from -> $to")

            val humanPlayers = players.filter(entry => !entry._1.startsWith("bot-"))
            humanPlayers.values.foreach(player => 
              player ! ServerMessages.BattleResult(
                gameId,
                from,
                to,
                battleResult.attackerDice.toList,
                battleResult.defenderDice.toList,
                battleResult.attackerLosses,
                battleResult.defenderLosses,
                battleResult.result == BattleResult.AttackerWins
              )
            )
            
        case msg =>
            log.warning(s"GameSession received unhandled message: $msg")    



    /*
     * Initialize the game engine with the provided players.
     * It separates human players from bot players, assigns colors, and creates the game engine instance
     */
    private def initializeGameEngine(players: List[Player]): Unit = {
      val humanPlayers = players.filter(!_.id.startsWith("bot-")).map { player =>
        CorePlayerImpl(
          player.id, 
          player.username, 
          generatePlayerColor(player.id),
          model.player.PlayerType.Human  
        )
      }

      var botController: Option[bot.RiskBotController] = None
      
      val botPlayers = 
        if (numBots > 0) {
          val existingBots = players.filter(_.id.startsWith("bot-"))
          val botStrategiesList = this.botStrategies.getOrElse(List.fill(numBots)("Offensivo"))
          println("=== BOT STRATEGIES ===")
          println(botStrategiesList.mkString(", "))
          
          existingBots.zipWithIndex.map { case (existingBot, index) =>
            val botColor = generatePlayerColor(existingBot.id)
            val botStrategy = if (index < botStrategiesList.length) botStrategiesList(index) else "Offensivo"
            
            println(s"Creazione bot ${existingBot.username} con strategia: $botStrategy")
            
            val (botPlayer, controller) = botStrategy match {
              case "Offensivo" => bot.BotFactory.createAggressiveBot(existingBot.id, existingBot.username, botColor)
              case "Difensivo" => bot.BotFactory.createDefensiveBot(existingBot.id, existingBot.username, botColor)
              case _ => bot.BotFactory.createAggressiveBot(existingBot.id, existingBot.username, botColor)
            }
            
            botController = Some(controller)
            
            CorePlayerImpl(
              existingBot.id,
              existingBot.username,
              botColor,
              model.player.PlayerType.Bot
            )
          }.toList
        } else {
          List.empty
        }
      
      val allPlayers = humanPlayers ++ botPlayers
      
      try {
        log.info(s"Initializing game engine for game $gameId with ${allPlayers.size} players (${humanPlayers.size} humans, ${botPlayers.size} bots)")
        gameEngine = Some(new GameEngine(allPlayers, gameId, botController))
        
        
        gameEngine.foreach { engine =>
          engine.setBattleResultCallback { (from, to, attackerId, defenderId, battleResult) =>
            log.info(s"Bot $attackerId ha eseguito un attacco: $from -> $to")

            self ! BotBattleNotification(from, to, attackerId, defenderId, battleResult)
          }
        }
      } catch {
        case ex: Exception => 
          log.error(s"Error initializing game engine: ${ex.getMessage}")
          gameEngine = None
      }
    }

    /*
      * Handle the creation or maintenance of bot players.
      * If there are existing bot players, they are kept as is.
     */
    private def manageBots(
        existingBotPlayers: Map[String, ActorRef], 
        existingBotPlayerData: Map[String, Player]
    ): (Map[String, ActorRef], Map[String, Player]) = {
      
      if (existingBotPlayers.nonEmpty) {
        println(s"Mantengo ${existingBotPlayers.size} bot esistenti")
        
        return (existingBotPlayers, existingBotPlayerData)
      }
      
      
      if (this.numBots > 0) {
        println(s"Creazione di ${this.numBots} nuovi bot")
        val botNamesResolved = this.botNames.getOrElse(List.fill(this.numBots)(s"Bot ${UUID.randomUUID().toString.take(4)}"))
        val botStrategiesResolved = this.botStrategies.getOrElse(List.fill(this.numBots)("Random"))
        
    
        var botPlayers = Map.empty[String, ActorRef]
        var botPlayerData = Map.empty[String, Player]
        
        for (i <- 0 until numBots) {
          val botId = s"bot-${UUID.randomUUID().toString.take(6)}"
          val botName = if (i < botNamesResolved.length) botNamesResolved(i) else s"Bot $i"
          val botStrategy = if (i < botStrategiesResolved.length) botStrategiesResolved(i) else "Random"
          
          
          val botDummyRef = context.system.deadLetters
          
          //mappe immutabili per i bot
          botPlayers = botPlayers + (botId -> botDummyRef)
          botPlayerData = botPlayerData + (botId -> Player(botId, botDummyRef, botName))
        }
        
        (botPlayers, botPlayerData)
      } else {
        // Nessun bot richiesto
        (Map.empty[String, ActorRef], Map.empty[String, Player])
      }
    }
    
    /*
     * Start the game by initializing the game engine and setting up the initial game state. 
     */
    private def startGame(): Unit = {
      gameEngine.foreach { engine =>
        try {
          log.info(s"Starting game $gameId")
          
          val finalState = engine.setup()

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


    /*
     * Converts a client action to a game action.
     */
    private def convertToGameAction(
        clientAction: ClientMessages.GameAction, 
        playerId: String
    ): engine.GameAction = {
      clientAction.action match {
        case "attack" => 
          val defenderId = clientAction.parameters.getOrElse("defenderId", "")
          val fromTerritory = clientAction.parameters.getOrElse("fromTerritory", "")
          val toTerritory = clientAction.parameters.getOrElse("toTerritory", "")
          val troops = clientAction.parameters.getOrElse("troops", "0").toInt
          
          println(s"=== DEBUG ATTACK ACTION ===")
          println(s"Attaccante: $playerId")
          println(s"Difensore: $defenderId")
          println(s"Da: $fromTerritory")
          println(s"A: $toTerritory")
          println(s"Truppe: $troops")
          println(s"Parametri ricevuti: ${clientAction.parameters}")
          println(s"==========================")
          
          engine.GameAction.Attack(
            playerId,
            defenderId,
            fromTerritory,
            toTerritory,
            troops
          )
          
        case "place_troops" =>
          engine.GameAction.PlaceTroops(
            playerId,
            clientAction.parameters.getOrElse("troops", "0").toInt,
            clientAction.parameters.getOrElse("territory", "")
          )

        case "trade_cards" =>
          val cardNames = clientAction.parameters.getOrElse("cards", "").split(",").map(_.trim).toSet
          engine.GameAction.TradeCards(playerId, cardNames)
          
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
    

    /*
     * Converts the game state from the engine to a format suitable for the client.
     * This includes converting player states, territories, and other relevant information.
     */
    private def convertGameStateToClient(gameState: engine.GameState): GameStateDto = {
      GameStateDto(
        gameId = gameState.gameId,
        currentPlayer = gameState.turnManager.currentPlayer.id,
        currentPhase = gameState.turnManager.currentPhase match {
          case TurnPhase.SetupPhase => "SetupPhase"  
          case TurnPhase.MainPhase => "MainPhase"
        },  
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
        }.toList,
        playerStartedTurn = gameState.playerStartedTurn.toString 
      )
    }

    /**
     * Extracts the target type and value from the objective card.
     * Returns a tuple with the target type as a string and the target value as a string.
     */
    private def extractObjectiveDetails(objective: model.cards.ObjectiveCard): (String, String) = {
      objective match {
        case ct: model.cards.ObjectiveCard.ConquerTerritories => 
          ("TERRITORY_COUNT", s"${ct.num}")
          
        case cc: model.cards.ObjectiveCard.ConquerContinents => 
          ("CONTINENTS", cc.continents.map(_.name).mkString(","))
          
        case _ => ("UNKNOWN", "")
      }
    }

    /**
     * Generates a unique color for the player based on their ID.
     * If the player already has a color assigned, returns that color.
     * If not, assigns the first available color from the PlayerColor enum.
     */
    private def generatePlayerColor(playerId: String): PlayerColor = {
      
      playerColors.get(playerId) match 
        case Some(color) => color
        case None =>
          
          val usedColors = playerColors.values.toSet
          val availableColors = PlayerColor.values.filterNot(usedColors.contains)
          
          val newColor = if (availableColors.nonEmpty) {
            // primo colore disponibile
            availableColors.head
          } else {
            
            val colors = PlayerColor.values
            val index = Math.abs(playerId.hashCode % colors.length)
            colors(index)
          }
          
          
          playerColors(playerId) = newColor
          newColor
      
    }
  
    private def getLastBattleAttackerDice(gameState: engine.GameState): List[Int] = {
      gameState.lastBattleResult match {
        case Some(battleResult) => battleResult.attackerDice.toList
        case None => 
          println("ATTENZIONE: Nessun risultato di battaglia trovato nel gameState, generando dadi casuali")
          scala.util.Random.shuffle((1 to 6).toList).take(3)
      }
    }
    
    private def getLastBattleDefenderDice(gameState: engine.GameState): List[Int] = {
      gameState.lastBattleResult match {
        case Some(battleResult) => battleResult.defenderDice.toList
        case None => 
          println("ATTENZIONE: Nessun risultato di battaglia trovato nel gameState, generando dadi casuali")
          scala.util.Random.shuffle((1 to 6).toList).take(2)
      }
    }
    /*
      * Returns the number of losses suffered by the attacker in the last battle.
     */
    private def getLastBattleAttackerLosses(gameState: engine.GameState): Int = {
      gameState.lastBattleResult match {
        case Some(battleResult) => battleResult.attackerLosses
        case None => 
          println("ATTENZIONE: Nessun risultato di battaglia trovato nel gameState, generando perdite casuali")
          scala.util.Random.nextInt(0)
      }
    }
    
    /*
      * Returns the number of losses suffered by the defender in the last battle.
     */
    private def getLastBattleDefenderLosses(gameState: engine.GameState): Int = {
      gameState.lastBattleResult match {
        case Some(battleResult) => battleResult.defenderLosses
        case None => 
          println("ATTENZIONE: Nessun risultato di battaglia trovato nel gameState, generando perdite casuali")
          scala.util.Random.nextInt(0)
      }
    }
    /**
     * Checks if the last battle was won by the attacker in the specified territory.
     * If no battle result is available, checks if the territory is owned by the attacker.
     * @return True if the last battle was won by the attacker, false otherwise
     */
    private def isLastBattleConquered(gameState: engine.GameState, territoryName: String, attackerId: String): Boolean = {
      gameState.lastBattleResult match {
        case Some(battleResult) => 
          battleResult.result == engine.BattleResult.AttackerWins
        case None => 
          gameState.board.territories.find(_.name == territoryName).exists(_.owner.exists(_.id == attackerId))
      }
    }
    
    /**
     * Executes bot turns sequentially until a human player is encountered.
     * For each bot turn completed, sends an update to human clients and
     * waits for a brief period to allow clients to visualize the actions.
     * @param engine The GameEngine on which to execute bot actions
     * @param currentBotId ID of the current bot to start with
     * @param humanPlayers Map of human players for sending notifications
     * @param playersList List of player names for messages
     * @return A tuple with the final game state and the ID of the next player
     */
    private def executeBotTurnsUntilHuman(
        engine: GameEngine, 
        currentBotId: String, 
        humanPlayers: Map[String, ActorRef],
        playersList: List[String]
    ): (GameState, String) = {
      println(s"[DEBUG] Esecuzione turno bot $currentBotId")
      try {
       
        val botGameState = engine.executeBotTurn()
        val nextPlayerId = botGameState.turnManager.currentPlayer.id
        
        println(s"[DEBUG] Bot $currentBotId ha completato il turno. Prossimo giocatore: $nextPlayerId")
        
       
        val intermediateClientState = convertGameStateToClient(botGameState)
        humanPlayers.values.foreach(player => 
          player ! ServerMessages.GameState(
            gameId, 
            playersList,
            nextPlayerId,
            scala.collection.immutable.Map("gameStateDto" -> intermediateClientState)  
          )
        )
        
        Thread.sleep(2000)
        
        val isNextPlayerBot = botGameState.turnManager.currentPlayer.playerType == model.player.PlayerType.Bot
        
        println(s"[DEBUG] Prossimo giocatore $nextPlayerId è un bot? $isNextPlayerBot")
        
        if (isNextPlayerBot) {
          println(s"[DEBUG] Il prossimo giocatore $nextPlayerId è ancora un bot, continuo la catena")
          //IMPORTANTE: bisogna usare il return esplicito per propagare il risultato della ricorsione
          return executeBotTurnsUntilHuman(engine, nextPlayerId, humanPlayers, playersList)
        } else {
          println(s"Il prossimo giocatore $nextPlayerId è umano, termino la catena")
          return (botGameState, nextPlayerId)
        }
      } catch {
        case e: exceptions.GameOverException => 
          println(s"[DEBUG] Game over durante il turno del bot $currentBotId: ${e.getMessage}")
          throw e
        case ex: Exception =>
          log.error(s"Errore durante l'esecuzione del turno del bot $currentBotId: ${ex.getMessage}")
          ex.printStackTrace()
          return (engine.getGameState, engine.getGameState.turnManager.currentPlayer.id)
      }
    }
  
end GameSession