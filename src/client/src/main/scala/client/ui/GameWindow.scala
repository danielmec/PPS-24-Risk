package client.ui

import scalafx.Includes._
import scalafx.application.Platform
import scalafx.geometry.Insets
import scalafx.geometry.Orientation
import scalafx.geometry.Pos
import scalafx.scene.Scene
import scalafx.scene.control._
import scalafx.scene.layout._
import scalafx.scene.text._
import scalafx.stage.Stage
import scalafx.collections.ObservableBuffer
import scala.math.Numeric.Implicits.infixNumericOps 
import client.ClientNetworkManager
import client.ClientJsonSupport._
import client.ui.components._
import client.ui.dialogs.TerritoriesDialog
import client.ui.dialogs.TroopPlacementDialog
import client.GameActionHandler
import client.AdapterMap
import client.AdapterMap.UITerritory
import model.board.Territory
import client.AdapterMap.UITerritory
import dialogs.AttackDialog
import dialogs.ReinforcementDialog
import model.cards.{TerritoryCard, CardImg}

class GameWindow(
  networkManager: ClientNetworkManager,
  gameId: String,
  gameName: String, 
  initPlayers: List[String],
  myUsername: String,  
  val myPlayerId: String,
  playerColors: Map[String, String] = Map.empty  
) extends Stage {
  
  println("IDENTITÀ GIOCATORE")
  println(s"Username: $myUsername")
  println(s"PlayerId: $myPlayerId")
  println(s"GameId: $gameId")
  println("------------------------")
  
  title = s"Risk - Partita: $gameName"
  width = 1100
  height = 800
  
  private var currentPlayerId: String = ""
  private var myObjective: Option[String] = None
  private var placementDialogOpen: Boolean = false
  private var currentPlacementDialog: Option[TroopPlacementDialog] = None
  var reinforcementDoneThisTurn: Boolean = false
  
  private var currentPhase: String = "SetupPhase"
  private val turnIndicator = new Label("")
  turnIndicator.style = "-fx-font-size: 14px; -fx-font-weight: bold; -fx-padding: 5px;"
  
  val territories = createTerritories()
  val actionHandler = new GameActionHandler(networkManager)(networkManager.executionContext)
  
  val titleLabel = new Label(s"Partita: $gameName")
  titleLabel.font = Font.font("Arial", FontWeight.Bold, 18)
  
  val playersLabel = new Label(s"Giocatori: ${initPlayers.size}")
  
  val playerInfoLabel = new Label(s"Giocatore: $myUsername")
  playerInfoLabel.font = Font.font("Arial", FontWeight.Bold, 14)
  playerInfoLabel.style = "-fx-text-fill: #2E8B57; -fx-background-color: #F0FFF0; -fx-padding: 5px; -fx-border-color: #2E8B57; -fx-border-width: 1px; -fx-border-radius: 3px; -fx-background-radius: 3px;"
  
  val topPane = new VBox(10) {
    padding = Insets(10)
    children = Seq(titleLabel, playerInfoLabel, turnIndicator, playersLabel)
  }
  
  val gameMapView = new GameMapView(() => showTerritoriesDialog())
  private val territoryInfoPane = new TerritoryInfoPane(territories)
  val actionPane = new ActionPane(showTerritoriesDialog())
  
  val centerPane = new VBox {
    children = Seq(gameMapView, territoryInfoPane, actionPane)
    VBox.setVgrow(gameMapView, Priority.Always)
  }
  
  val playersList = ObservableBuffer(initPlayers.map(name => 
    new PlayerInfoView(name, initPlayers, networkManager, playerColors)): _*)

  val diceDisplay = new DiceDisplay()
  
  val sidebarPane = createSidebarPane()
  
  val splitPane = new SplitPane {
    orientation = Orientation.Horizontal
    items.addAll(centerPane, sidebarPane)
    dividerPositions = 0.75
    VBox.setVgrow(this, Priority.Always)
  }
  
  val showObjectiveButton = new Button("Mostra Obiettivo")
  showObjectiveButton.disable = true
  showObjectiveButton.onAction = handle {
    println("Pulsante obiettivo cliccato!")
    showObjectiveDialog()
  }

  val leaveButton = new Button("Abbandona Partita")
  leaveButton.onAction = handle {
    networkManager.sendMessage(LeaveGameMessage(gameId)).onComplete {
      case _ => Platform.runLater {
        close()
      }
    }(networkManager.executionContext)
  }
  
  val bottomPane = new HBox(10) {
    padding = Insets(10)
    alignment = Pos.CenterRight
    children = Seq(showObjectiveButton, leaveButton)
  }
  
  val root = new BorderPane()
  root.top = topPane
  root.center = splitPane
  root.bottom = bottomPane
  scene = new Scene(root)
  
  gameMapView.bindToSceneDimensions(scene.width.value.doubleValue(), scene.height.value.doubleValue())
  
  registerCallbacks()

  setupUIEventHandlers()


  private var waitAlert: Option[Alert] = None

  showWaitingForPlayersAlert()


  /**
   * It registers the callbacks for various game events.
   **/
  private def registerCallbacks(): Unit = {
    networkManager.registerCallback("gameStarted", msg => {
      println(s"Callback gameStarted ricevuto!")
      handleGameStarted(msg.asInstanceOf[GameStartedMessage])
    })
    
    networkManager.registerCallback("gameState", msg => {
      println(s"Callback gameState ricevuto!")
      println(s"Sono: $myUsername ($myPlayerId)")
      
      val gameState = msg.asInstanceOf[GameState]
      currentPhase = gameState.state.currentPhase
      println(s"[FASE AGGIORNATA] La fase corrente è: $currentPhase")
      handleGameState(gameState)
    })
    
    networkManager.registerCallback("battleResult", msg => {
      println(s"Callback battleResult ricevuto!")
      val battleResult = msg.asInstanceOf[BattleResultMessage]
      handleBattleResult(battleResult)
    })
    
    networkManager.registerCallback("troopMovement", msg => {
      println(s"Callback troopMovement ricevuto!")
      val movement = msg.asInstanceOf[TroopMovementMessage]
      handleTroopMovement(movement)
    })
    
    networkManager.registerCallback("gameOver", msg => {
      println(s"Callback gameOver ricevuto!")
      val gameOver = msg.asInstanceOf[GameOverMessage]
      handleGameOver(gameOver)
    })

    networkManager.registerCallback("playerLeft", msg => {
      println(s"Callback playerLeft ricevuto!")
      val playerLeftMsg = msg.asInstanceOf[PlayerLeftMessage]
      handlePlayerLeft(playerLeftMsg)
    })
    
    networkManager.registerCallback("gameJoined", msg => {
      msg match {
        case GameJoinedMessage(gameId, players, gameName, playerColors) =>
          if (gameId == this.gameId) {
            Platform.runLater {
              println(s"Aggiornamento lista giocatori tramite gameJoined: ${players.mkString(", ")}")
              
              updatePlayers(players, playerColors.getOrElse(Map.empty))
            }
          } else {
            println(s"Ignorato messaggio gameJoined per partita $gameId (sono nella ${this.gameId})")
          }
          
        case _ => println("Messaggio gameJoined ricevuto con formato non valido")
      }
    })

     networkManager.registerCallback("trisPlayed", msg => {
      val trisMsg = msg.asInstanceOf[TrisPlayedMessage]
      if (trisMsg.playerId == myPlayerId) {
        Platform.runLater {
          val alert = new Alert(Alert.AlertType.Information) {
            initOwner(GameWindow.this)
            title = "Tris giocato"
            headerText = None
            contentText = s"Hai giocato un tris! Bonus ricevuto: ${trisMsg.bonus} truppe."
          }
          alert.showAndWait()
        }
      }
    })

  }
    

  /** 
   * Setup the UI event handlers for the action buttons.
   */
  private def setupUIEventHandlers(): Unit = {
    actionPane.endTurnButton.onAction = handle {
      actionPane.endTurnButton.disable = true
      
      println(s"[UI] Fine turno richiesto dall'utente (fase corrente: $currentPhase)")
      
      val actionFuture = if (currentPhase == "SetupPhase") {
        println("[UI] Inviando end_setup al server")
        actionHandler.endSetup(gameId)
      } else {
        println("[UI] Inviando end_turn al server")
        actionHandler.endTurn(gameId)
      }
      
      actionFuture.onComplete {
        case scala.util.Success(true) =>
          println("[UI] Azione inviata con successo al server")
        case scala.util.Success(false) =>
          Platform.runLater {
            val alert = new Alert(Alert.AlertType.Warning) {
              initOwner(GameWindow.this)
              title = "Azione non consentita"
              headerText = "Impossibile terminare il turno"
              contentText = "Il server ha rifiutato la richiesta."
            }
            alert.showAndWait()
            actionPane.endTurnButton.disable = false
          }
        case scala.util.Failure(ex) =>
          Platform.runLater {
            val alert = new Alert(Alert.AlertType.Error) {
              initOwner(GameWindow.this)
              title = "Errore"
              headerText = "Errore di comunicazione"
              contentText = s"Errore durante la richiesta: ${ex.getMessage}"
            }
            alert.showAndWait()
            actionPane.endTurnButton.disable = false
          }
      }(networkManager.executionContext)
    }
    
    actionPane.attackButton.onAction = handle {
      val myTerritories = territories.filter(_.owner.value == myPlayerId)
      if (myTerritories.isEmpty) {
        showErrorAlert("Non hai territori da cui attaccare")
      }
      
      val attackDialog = new AttackDialog(
        this,
        myTerritories,
        territories.filter(_.owner.value != myPlayerId)
      )
      
      val result = attackDialog.showAndWaitWithResult() 
      result.foreach { attackInfo =>
        actionPane.attackButton.disable = true
        actionHandler.attack(
          gameId,
          attackInfo.fromTerritory,
          attackInfo.toTerritory,
          attackInfo.troops,
          attackInfo.defenderId
        ).onComplete {
          case scala.util.Success(true) =>
            println("[UI] Attacco inviato con successo")
            Platform.runLater {
              actionPane.attackButton.disable = false
            }
          case _ =>
            Platform.runLater {
              actionPane.attackButton.disable = false
              showErrorAlert("Errore nell'esecuzione dell'attacco")
            }
        }(networkManager.executionContext)
      }
    }

    actionPane.reinforceButton.onAction = handle {
      showReinforcementDialog()
    }

    actionPane.cardsButton.onAction = handle {
      val cards: Seq[client.ui.dialogs.CardInfo] = {
        val stateOpt = networkManager.getLastGameState()
        stateOpt.flatMap { gs =>
          gs.state.playerStates.find(_("playerId") == myPlayerId)
            .flatMap { playerState =>
              playerState.get("territoryCards").map { raw =>
                val regex = """\{([^}]+)\}""".r
                regex.findAllIn(raw.toString).toList.map { cardStr =>
                  val fields = cardStr.stripPrefix("{").stripSuffix("}").split(",").map(_.split(":", 2)).collect {
                    case Array(k, v) => k.trim -> v.trim
                  }.toMap
                  val cardTypeRaw = fields.getOrElse("cardType", "Infantry").capitalize
                  val cardImg = cardTypeRaw match {
                    case "Infantry"  => CardImg.Infantry
                    case "Cavalry"   => CardImg.Cavalry
                    case "Artillery" => CardImg.Artillery
                    case "Jolly"     => CardImg.Jolly
                    case _           => CardImg.Infantry // fallback di sicurezza
                  }
                  client.ui.dialogs.CardInfo(
                    fields.getOrElse("territoryName", "???"),
                    fields.getOrElse("cardType", "???")
                  )
                }
              }
            }
        }.getOrElse(Seq.empty)
      }
      val dialog = new client.ui.dialogs.ShowCardsDialog(this, cards)
      dialog.showAndWait()
    }
  }


  /*
   * Show Error Alert
   */
  private def showErrorAlert(message: String): Unit = {
    Platform.runLater {
      val alert = new Alert(Alert.AlertType.Error) {
        initOwner(GameWindow.this)
        title = "Errore"
        headerText = None
        contentText = message
      }
      alert.showAndWait()
    }
  }
  

  /*
    * Create a Dialog to show the reinforcement options.
   */
  private def showTroopPlacementDialog(myTerritories: ObservableBuffer[UITerritory], bonusTroops: Int): Unit = {
    if (!placementDialogOpen && myTerritories.nonEmpty) {
      try {
        println("Tentativo di creare e mostrare il dialogo di piazzamento...")

        // Estrai le carte territorio dal playerState
        val territoryCards: Seq[TerritoryCard] = {
          val stateOpt = networkManager.getLastGameState()
          stateOpt.flatMap { gs =>
            gs.state.playerStates.find(_("playerId") == myPlayerId)
              .flatMap { playerState =>
                playerState.get("territoryCards").map { raw =>
                  // Adatta questo parsing al formato reale delle tue carte!
                  val regex = """\{([^}]+)\}""".r
                  regex.findAllIn(raw.toString).toList.map { cardStr =>
                    val fields = cardStr.stripPrefix("{").stripSuffix("}").split(",").map(_.split(":", 2)).collect {
                      case Array(k, v) => k.trim -> v.trim
                    }.toMap
                    val cardTypeRaw = fields.getOrElse("cardType", "Infantry").capitalize
                    val cardImg = cardTypeRaw match {
                      case "Infantry"  => CardImg.Infantry
                      case "Cavalry"   => CardImg.Cavalry
                      case "Artillery" => CardImg.Artillery
                      case "Jolly"     => CardImg.Jolly
                      case _           => CardImg.Infantry // fallback di sicurezza
                    }
                    println(s"[DEBUG] Card parsed: name=${fields.getOrElse("territoryName", "???")}, type=$cardTypeRaw, cardImg=$cardImg")
                    // Adatta qui se hai un costruttore diverso!
                    TerritoryCard(
                      territory = model.board.Territory(
                        name = fields.getOrElse("territoryName", "???"),
                        owner = None,           // Non hai info sull'owner per la carta
                        troops = 0,             // Non ti serve il numero di truppe per la carta
                        neighbors = Set.empty   // Non ti servono i vicini per la carta
                      ),
                      cardImg = cardImg
                    )
                  }
                }
              }
          }.getOrElse(Seq.empty)
        }

        val placementDialog = new TroopPlacementDialog(
          this,
          myTerritories,
          bonusTroops,
          currentPhase,
          territoryCards // <-- passa le carte territorio qui!
        )

        placementDialogOpen = true
        currentPlacementDialog = Some(placementDialog)

        placementDialog.onHidden = _ => {
          placementDialogOpen = false
          currentPlacementDialog = None
        }

        Platform.runLater {
          placementDialog.show()
          placementDialog.toFront()
          println("Dialog visualizzato e portato in primo piano")
        }
      } catch {
        case e: Exception =>
          println(s"ERRORE nella creazione del dialogo: ${e.getMessage}")
          e.printStackTrace()
          placementDialogOpen = false
          currentPlacementDialog = None
      }
    } else if (placementDialogOpen) {
      currentPlacementDialog.foreach { dialog =>
        println("Aggiornamento truppe nel dialogo esistente")
        Platform.runLater {
          dialog.updateTroops(bonusTroops)
        }
      }
    }
  }
  
  /*
    * Close the troop placement dialog if it is open.
    * This is called when the turn ends or when the dialog is no longer needed.
  */
  private def closeTroopPlacementDialog(): Unit = {
    if (placementDialogOpen) {
      currentPlacementDialog.foreach { dialog =>
        Platform.runLater {
          dialog.close()
        }
      }
    }
  }
  

  /*
    * Handle the game state update received from the server.
   */
  private def handleGameState(gameState: GameState): Unit = {
    Platform.runLater {
      println(s"Aggiornamento stato di gioco. Fase: ${gameState.state.currentPhase}")

      gameState.state.territories.foreach { territoryMap =>
        val name = territoryMap.getOrElse("name", "")
        val owner = territoryMap.getOrElse("owner", "")
        val troops = territoryMap.getOrElse("troops", "0").toInt
        
        updateTerritory(name, owner, troops)
      }
    
      territoryInfoPane.updatePlayerTerritories(myPlayerId)
      territoryInfoPane.updateContinentControl(myPlayerId)
      
      val isMyTurn = myPlayerId == gameState.state.currentPlayer
      val currentPhase = gameState.state.currentPhase
      
      val currentPlayerName = gameState.players.find(p => p.contains(gameState.state.currentPlayer))
        .map(p => p.split(" \\(").head)
        .getOrElse("Sconosciuto")
      
      if (isMyTurn) {
        turnIndicator.text = s"È IL TUO TURNO - Fase: $currentPhase"
        turnIndicator.style = "-fx-text-fill: green; -fx-font-size: 14px; -fx-font-weight: bold; -fx-padding: 5px; -fx-background-color: #e8ffe8; -fx-border-color: green; -fx-border-width: 1px; -fx-border-radius: 3px;"
      } else {
        turnIndicator.text = s"Turno di $currentPlayerName - Fase: $currentPhase"
        turnIndicator.style = "-fx-text-fill: #555; -fx-font-size: 14px; -fx-font-weight: bold; -fx-padding: 5px; -fx-background-color: #f8f8f8; -fx-border-color: #aaa; -fx-border-width: 1px; -fx-border-radius: 3px;"
      }

      if (isMyTurn) {
        val myPlayerState = gameState.state.playerStates.find(_.getOrElse("playerId", "") == myPlayerId)
        
        myPlayerState.foreach { playerState =>
          val bonusTroops = playerState.getOrElse("bonusTroops", "0").toInt
          actionPane.cardsButton.disable = false

          if (currentPhase == "SetupPhase") {
            actionPane.getAttackButton.disable = true
            actionPane.getReinforceButton.disable = true
            actionPane.getEndTurnButton.disable = false
            
            val myTerritories = territories.filter(t => t.owner.value == myPlayerId)
            showTroopPlacementDialog(myTerritories, bonusTroops)
          } 
          else if (currentPhase == "MainPhase") {
            if (bonusTroops > 0 && gameState.state.playerStartedTurn == "true") {
              actionPane.getAttackButton.disable = true
              actionPane.getReinforceButton.disable = true
              actionPane.getEndTurnButton.disable = true
              
              println(s"[handleGameState] Mostro dialogo piazzamento - Inizio turno: ${gameState.state.playerStartedTurn}, bonus: $bonusTroops")
              val myTerritories = territories.filter(t => t.owner.value == myPlayerId)
              showTroopPlacementDialog(myTerritories, bonusTroops)
            } 
            else {
              closeTroopPlacementDialog()
              actionPane.getAttackButton.disable = false
              actionPane.getReinforceButton.disable = false
              actionPane.getEndTurnButton.disable = false
            }
          }
        }
      } 
      else {
        closeTroopPlacementDialog()
        actionPane.getAttackButton.disable = true
        actionPane.getReinforceButton.disable = true
        actionPane.getEndTurnButton.disable = true
        actionPane.cardsButton.disable = true
      }
    }

  }
  
  /*
    * Handle the game started message from the server.
    * This will initialize the game state and update the UI accordingly. 
  */
  def handleGameStarted(gameStartedMsg: GameStartedMessage): Unit = {

    waitAlert.foreach(alert => Platform.runLater {
      alert.close()
      waitAlert = None
    })
    
    Platform.runLater {
  
      println(s"Sono: $myUsername ($myPlayerId)")
      println(s"Turno di: ${gameStartedMsg.currentPlayerId}")

      currentPlayerId = gameStartedMsg.currentPlayerId
      
      val initialState = gameStartedMsg.initialState
      val stateData = initialState.state
      
      println(s"Fase corrente: ${stateData.currentPhase}")
      println(s"Giocatore corrente: ${stateData.currentPlayer}")
      println(s"Territori trovati: ${stateData.territories.size}")
      println(s"Stati giocatore trovati: ${stateData.playerStates.size}")
      
      stateData.territories.foreach { territoryMap =>
        val name = territoryMap.getOrElse("name", "")
        val owner = territoryMap.getOrElse("owner", "")
        val troops = territoryMap.getOrElse("troops", "0").toInt
        
        updateTerritory(name, owner, troops)
      }
      
      val myPlayerState = stateData.playerStates.find(_.getOrElse("playerId", "") == myPlayerId)
      
      println(s"Ho trovato il mio stato? ${myPlayerState.isDefined}")
      
      myPlayerState.foreach { playerState =>
        val missionCard = playerState.get("missionCard")
        println(s"Carta missione: $missionCard")
        
        val missionDesc = missionCard match {
          case Some(desc) => desc
          case None => "Nessun obiettivo assegnato"
        }
        
        myObjective = missionDesc match {
          case "Nessun obiettivo assegnato" => None
          case _ => Some(missionDesc)
        }
        showObjectiveButton.disable = myObjective.isEmpty
        
        println(s"Obiettivo estratto: $missionDesc")
        println(s"Pulsante obiettivo abilitato: ${!showObjectiveButton.disable.value}")
        
        val bonusTroops = playerState.getOrElse("bonusTroops", "0").toInt
        
        if (myPlayerId == stateData.currentPlayer && 
           (stateData.currentPhase == "MainPhase" || stateData.currentPhase == "SetupPhase") && 
           bonusTroops > 0) {
           
          println(s"[handleGameState] CONTROLLO INIZIO TURNO - playerStartedTurn: '${stateData.playerStartedTurn}'")
          
          if (stateData.playerStartedTurn == "true") { 
            val myTerritories = territories.filter(t => t.owner.value == myPlayerId)
            println(s"Territori del giocatore ${myPlayerId}: ${myTerritories.size}")
            println(s"Inizio turno: ${stateData.playerStartedTurn} - Mostrando dialogo piazzamento truppe")
            
            showTroopPlacementDialog(myTerritories, bonusTroops)
          } else {
            println(s"[handleGameState] NON mostro dialogo perché playerStartedTurn = '${stateData.playerStartedTurn}'")
          }
        } else if (stateData.currentPhase == "MainPhase" || stateData.currentPhase == "SetupPhase") {
          Platform.runLater {
            val currentPlayerName = initialState.players.find(p => p.contains(stateData.currentPlayer))
              .map(p => p.split(" \\(").head)
              .getOrElse("altro giocatore")
              
            val waitAlert = new Alert(Alert.AlertType.Information) {
              initOwner(GameWindow.this)
              title = "In attesa..."
              headerText = "Fase di Piazzamento Truppe"
              contentText = s"In attesa che $currentPlayerName piazzi le sue truppe..."
            }
            waitAlert.show()
          }
        }
      }

      territoryInfoPane.updatePlayerTerritories(myPlayerId)
      territoryInfoPane.updateContinentControl(myPlayerId)
      
      println(s"Territori dopo l'aggiornamento: ${territories.size}")
      println(s"Territori del giocatore ${myPlayerId}: ${territories.count(_.owner.value == myPlayerId)}")
    }
  }
  
  /*
  * Handle the battle result received from the server.
  * This will update the UI accordingly.
  */
    
  def showTerritoriesDialog(): Unit = {
    //mappa id -> username dai giocatori
    val playerIdToUsername = playersList.map { playerInfoView =>
      val playerInfo = playerInfoView.nameLabel.text.value
      //estrae l'ID tra parentesi alla fine del testo "username (id)"
      val idPattern = ".*\\((.+?)\\)$".r
      val playerId = playerInfo match {
        case idPattern(id) => id
        case _ => playerInfo
      }
      val username = playerInfo.split(" \\(").headOption.getOrElse(playerInfo)
      playerId -> username
    }.toMap
    
    //passa la mappa al dialog
    val dialog = new TerritoriesDialog(this, territories, playerIdToUsername)
    dialog.showAndWait()
  }

  /*
  * Show the objective dialog with the player's secret objective.
  */
  def showObjectiveDialog(): Unit = {
    myObjective.foreach { objective =>
      val dialog = new Alert(Alert.AlertType.Information) {
        initOwner(GameWindow.this)
        title = "Il Tuo Obiettivo"
        headerText = "Obiettivo Segreto"
        contentText = objective
      }
      dialog.showAndWait()
    }
  }

  private def createEmptyTerritory(name: String): UITerritory = {
    val emptyTerritory = Territory(
      name = name,
      neighbors = Set.empty,
      owner = None,
      troops = 0
     )
      
    new UITerritory(emptyTerritory, "Sconosciuto")
  }

  /*
    * Get the game ID of the current game.
    * This is used to identify the game in network messages.
   */
  def getGameId: String = gameId
  

  /**
   * Handle the player left message.
   */
  def handlePlayerLeft(msg: PlayerLeftMessage): Unit = {
    Platform.runLater {
      val playerId = msg.player
      println(s"Il giocatore ${msg.player} ha lasciato la partita.")
      
      val dialog = new Alert(Alert.AlertType.Information) {
        initOwner(GameWindow.this)
        title = "Giocatore Uscito"
        headerText = "Un giocatore ha lasciato la partita"
        contentText = s"$playerId ha lasciato la partita."
      }
      dialog.showAndWait()
 
      close()
    }
  }

  /*
    * Update the players list and their colors.
    * This will recreate the players list and update the sidebar pane. 
   */
  def updatePlayers(newPlayers: List[String], newColors: Map[String, String] = playerColors): Unit = {
    Platform.runLater {
      playersLabel.text = s"Giocatori: ${newPlayers.size}"
      
      // Debug: stampiamo la lista dei colori ricevuti dal server
      println("\n=== DEBUG COLORI GIOCATORI ===")
      println(s"Colori ricevuti dal server: ${newColors}")
      
      newPlayers.foreach { playerInfo =>
        // Estrai l'ID del giocatore
        val idPattern = ".*\\((.+?)\\)$".r
        val playerId = playerInfo match {
          case idPattern(id) => id
          case _ => playerInfo
        }
        
        // Stampa l'ID del giocatore e il suo colore
        val colorName = newColors.getOrElse(playerId, "NON TROVATO")
        println(s"Giocatore: $playerInfo (ID: $playerId) -> Colore: $colorName")
      }
      println("============================\n")
      
      val updatedPlayersList = ObservableBuffer(newPlayers.map(name => 
        new PlayerInfoView(name, newPlayers, networkManager, newColors)): _*)
      
      val newSidebarPane = createSidebarPane(updatedPlayersList)
      splitPane.items.set(1, newSidebarPane)
      
      playersList.clear()
      playersList.addAll(updatedPlayersList)
      
      println(s"Lista giocatori ricreata con: ${newPlayers.mkString(", ")}")
    }
  }
  

  /* 
    *  Update the territory with the given name, owner, and troops.
   */
  def updateTerritory(name: String, owner: String, troops: Int): Unit = {
    val territory = territories.find(_.name == name)

    
    territory match {
      case Some(t) => 
        val wasMyTerritory = t.owner.value == myPlayerId
        val becomesMyTerritory = owner == myPlayerId
        
        t.owner.value = owner
        t.armies.value = troops
        
      case None => 
        val newTerritory = createEmptyTerritory(name)
        newTerritory.owner.value = owner
        newTerritory.armies.value = troops
        territories += newTerritory
        
    }
  }

  def updateDiceValues(attackerValues: List[Int], defenderValues: List[Int]): Unit = {
    diceDisplay.updateValues(attackerValues, defenderValues)
  }
  
  private def createSidebarPane(players: ObservableBuffer[PlayerInfoView] = playersList): VBox = {
    new VBox(15) {
      padding = Insets(10)
      style = "-fx-background-color: #e8e8e8;"
      
      children = Seq(
        new Label("Giocatori") {
          font = Font.font("Arial", FontWeight.Bold, 16)
          textAlignment = TextAlignment.Center
          alignment = Pos.Center
          maxWidth = Double.MaxValue
        },
        new Separator(),
        new VBox(10) {
          children = players
          VBox.setVgrow(this, Priority.Always)
        },
        new Separator(),
        new Label("Dadi") {
          font = Font.font("Arial", FontWeight.Bold, 16)
          textAlignment = TextAlignment.Center
          alignment = Pos.Center
          maxWidth = Double.MaxValue
        },
        diceDisplay
      )
      
      maxWidth = 250
      prefWidth = 250
    }
  }
  

  /*  
   *  Create the territories list from the AdapterMap.
   *  This will load the territories and their initial state.
   */
  private def createTerritories(): ObservableBuffer[UITerritory] = {
    AdapterMap.loadTerritories()
  }
  
  /*  
   *  Handle the result of a battle.
   *  This will update the dice display and show an alert with the battle outcome.
   */
  private def handleBattleResult(battleResult: BattleResultMessage): Unit = {
    println(s"[UI] Ricevuto risultato battaglia: ${battleResult}")

    Platform.runLater {
      try {
        println(s"[UI] diceDisplay: visibile=${diceDisplay.visible.value}, width=${diceDisplay.width.value}, height=${diceDisplay.height.value}")
        
        diceDisplay.visible = true
        diceDisplay.opacity = 1.0
        diceDisplay.managed = true
        
        println(s"[UI] Aggiorno i dadi con: Attaccante=${battleResult.attackerDice.mkString(",")}, Difensore=${battleResult.defenderDice.mkString(",")}")
        diceDisplay.updateValues(battleResult.attackerDice, battleResult.defenderDice)
        

        val originalStyle = diceDisplay.style.value
        diceDisplay.style = originalStyle + "-fx-background-color: #ffcccc;"
        
        val timer = new java.util.Timer()
        timer.schedule(new java.util.TimerTask {
          override def run(): Unit = {
            Platform.runLater {
              diceDisplay.style = originalStyle
            }
          }
        }, 500)
        
        val outcomeText = if (battleResult.conquered) 
          s"Territorio ${battleResult.defenderTerritory} conquistato!" 
        else 
          s"Attacco a ${battleResult.defenderTerritory} respinto."
        
        val attackerId = territories.find(_.name == battleResult.attackerTerritory)
          .map(_.owner.value).getOrElse("Sconosciuto")

        val attackerName = playersList.map(_.nameLabel.text.value)
          .find(_.contains(s"($attackerId)"))
          .getOrElse("Sconosciuto")

        val attackMessage = s"Attacco di ${attackerName} da ${battleResult.attackerTerritory} a ${battleResult.defenderTerritory}"
        val lossesMessage = s"$outcomeText\nPerdite: Attaccante ${battleResult.attackerLosses}, Difensore ${battleResult.defenderLosses}"
          
        val alert = new Alert(Alert.AlertType.Information) {
            initOwner(GameWindow.this)
            title = "Risultato Battaglia"
            headerText = attackMessage
            contentText = lossesMessage
          }
        alert.show()
        
      } catch {
        case ex: Exception =>
          println(s"[UI] ERRORE durante la gestione del risultato battaglia: ${ex.getMessage}")
          ex.printStackTrace()
      }
    }
  }
  
  /*  
   *  Handle troop movement messages.
   *  This will show an alert with the movement details if the player is not the one who moved the troops.
   */
  private def handleTroopMovement(movement: TroopMovementMessage): Unit = {
    Platform.runLater {
      if (movement.playerId != myPlayerId) {
        val alert = new Alert(Alert.AlertType.Information) {
          initOwner(GameWindow.this)
          title = "Spostamento Truppe"
          headerText = None
          contentText = s"${movement.playerId} ha spostato ${movement.troops} truppe da ${movement.fromTerritory} a ${movement.toTerritory}"
        }
        alert.show()
      }
    }
  }

  /*  
   *  Show the reinforcement dialog if the player has territories with more than 1 army.
   *  Otherwise, show an alert indicating that no valid territories are available for reinforcement.
   */
  private def showReinforcementDialog(): Unit = {
    val myTerritories = territories.filter(t => t.owner.value == myPlayerId)
    if (myTerritories.exists(_.armies.value > 1)) {
      val dialog = new client.ui.dialogs.ReinforcementDialog(this, myTerritories)
      dialog.showAndWait()
    } else {
      val alert = new Alert(Alert.AlertType.Information) {
        initOwner(GameWindow.this)
        title = "Rinforzo non possibile"
        headerText = "Nessun territorio valido"
        contentText = "Non hai territori con abbastanza truppe per spostare rinforzi."
      }
      alert.showAndWait()
    }
  }

  /*  
   *  Handle the game over message.
   *  This will show an alert indicating the winner and close the game window.
   */
  private def handleGameOver(gameOver: GameOverMessage): Unit = {
    Platform.runLater {
      val winnerIsCurrentPlayer = gameOver.winnerId == myPlayerId
      
      val alert = new Alert(
        if (winnerIsCurrentPlayer) Alert.AlertType.Confirmation else Alert.AlertType.Information
      ) {
        initOwner(GameWindow.this)
        title = "Fine Partita"
        headerText = if (winnerIsCurrentPlayer) "Hai vinto!" else "Partita Terminata"
        contentText = if (winnerIsCurrentPlayer) 
          s"Congratulazioni! Hai completato il tuo obiettivo e vinto la partita!" 
        else 
          s"Il giocatore ${gameOver.winnerUsername} ha vinto la partita completando il suo obiettivo."
        
        dialogPane().style = "-fx-background-color: #f0f8ff;"
      }
      
      alert.showAndWait()
      
      close()
    }
  }
  
  /**
    * Show an alert indicating that the game is waiting for more players to join.
    * This is shown when the game is not yet full and waiting for more players.
    */
  private def showWaitingForPlayersAlert(): Unit = {
    Platform.runLater {
      val alert = new Alert(Alert.AlertType.Information) {
        initOwner(GameWindow.this)
        title = "In attesa"
        headerText = "Lobby non completa"
        contentText = "In attesa che altri giocatori si uniscano alla partita..."
        
        dialogPane().style = "-fx-background-color: #f8f8ff;"
        
        x = GameWindow.this.x.value + 50
        y = GameWindow.this.y.value + 50
      }
      waitAlert = Some(alert)
      alert.show()
    }
  }

}