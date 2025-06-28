package client.ui

import scalafx.Includes._
import scalafx.application.Platform
import scalafx.geometry.{Insets, Pos, Orientation}
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
import client.ui.dialogs.AttackDialog
import client.GameActionHandler
import client.AdapterMap
import client.AdapterMap.UITerritory
import model.board.Territory
import client.AdapterMap.UITerritory

class GameWindow(
  networkManager: ClientNetworkManager,
  gameId: String,
  gameName: String, 
  initPlayers: List[String],
  myUsername: String,  
  val myPlayerId: String 
) extends Stage {
  
  println("IDENTITÀ GIOCATORE")
  println(s"Username: $myUsername")
  println(s"PlayerId: $myPlayerId")
  println(s"GameId: $gameId")
  println("------------------------")
  
  title = s"Risk - Partita: $gameName"
  width = 1100
  height = 800
  
  // ==================== VARIABILI DI STATO ==================== 
  private var currentPlayerId: String = ""
  private var myObjective: Option[String] = None
  private var placementDialogOpen: Boolean = false
  private var attackDialogOpen: Boolean = false
  private var currentPlacementDialog: Option[TroopPlacementDialog] = None
  var reinforcementDoneThisTurn: Boolean = false
  
  // ==================== COMPONENTI UI ====================
  //Inizializzazione dei territori
  val territories = createTerritories()
  val actionHandler = new GameActionHandler(networkManager)(networkManager.executionContext)
  
  val titleLabel = new Label(s"Partita: $gameName")
  titleLabel.font = Font.font("Arial", FontWeight.Bold, 18)
  
  val playersLabel = new Label(s"Giocatori: ${initPlayers.size}")
  
  // Panel per mostrare il nome del giocatore corrente
  val playerInfoLabel = new Label(s"Giocatore: $myUsername")
  playerInfoLabel.font = Font.font("Arial", FontWeight.Bold, 14)
  playerInfoLabel.style = "-fx-text-fill: #2E8B57; -fx-background-color: #F0FFF0; -fx-padding: 5px; -fx-border-color: #2E8B57; -fx-border-width: 1px; -fx-border-radius: 3px; -fx-background-radius: 3px;"
  
  val topPane = new VBox(10) {
    padding = Insets(10)
    children = Seq(titleLabel, playerInfoLabel, playersLabel)
  }
  
  val gameMapView = new GameMapView(() => showTerritoriesDialog())
  private val territoryInfoPane = new TerritoryInfoPane(territories)
  val actionPane = new ActionPane(showTerritoriesDialog())
  
  val centerPane = new VBox {
    children = Seq(gameMapView, territoryInfoPane, actionPane)
    VBox.setVgrow(gameMapView, Priority.Always)
  }
  
  // Barra laterale
  val playersList = ObservableBuffer(initPlayers.map(name => new PlayerInfoView(name, initPlayers, networkManager)): _*)
  val diceDisplay = new DiceDisplay()
  
  val sidebarPane = createSidebarPane()
  
  val splitPane = new SplitPane {
    orientation = Orientation.Horizontal
    items.addAll(centerPane, sidebarPane)
    dividerPositions = 0.75 // 75% mappa, 25% barra laterale
    VBox.setVgrow(this, Priority.Always)
  }
  
  // Barra inferiore
  val showObjectiveButton = new Button("Mostra Obiettivo")
  showObjectiveButton.disable = true
  showObjectiveButton.onAction = handle {
    println("Pulsante obiettivo cliccato!")
    showObjectiveDialog()
  }

  val leaveButton = new Button("Abbandona partita")
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
  
  // Layout principale
  val root = new BorderPane()
  root.top = topPane
  root.center = splitPane
  root.bottom = bottomPane
  
  scene = new Scene(root)
  
  // Binding delle dimensioni
  gameMapView.bindToSceneDimensions(scene.width.value.doubleValue(), scene.height.value.doubleValue())
  
  // ==================== REGISTRAZIONE CALLBACK ====================
  registerCallbacks()
  
  // ==================== COLLEGAMENTO EVENTI UI ====================
  setupUIEventHandlers()
  
  // ==================== METODI ====================
  
  /**
   * Registra i callback per i messaggi dal server
   */
  private def registerCallbacks(): Unit = {
    networkManager.registerCallback("gameStarted", msg => {
      println(s"Callback gameStarted ricevuto!")
      handleGameStarted(msg.asInstanceOf[GameStartedMessage])
    })
    
    networkManager.registerCallback("gameState", msg => {
      println(s"Callback gameState ricevuto!")
      println(s"Sono: $myUsername ($myPlayerId)")
      
      val gameState = msg.asInstanceOf[GameState]
      println(s"Turno aggiornato: ${gameState.state.currentPlayer}")
      println(s"È il mio turno? ${gameState.state.currentPlayer == myPlayerId}")
      
      handleGameState(gameState)
    })
    
    networkManager.registerCallback("gameJoined", msg => {
      msg match {
        case GameJoinedMessage(gameId, players, gameName) =>
          if (gameId == this.gameId) {
            Platform.runLater {
              println(s"Aggiornamento lista giocatori tramite gameJoined: ${players.mkString(", ")}")
              updatePlayers(players)
            }
          } else {
            println(s"Ignorato messaggio gameJoined per partita $gameId (sono nella ${this.gameId})")
          }
          
        case _ => println("Messaggio gameJoined ricevuto con formato non valido")
      }
    })

    networkManager.registerCallback("battleResult", msg => {
      println(s"Callback battleResult ricevuto!")
      val battleResult = msg.asInstanceOf[BattleResultMessage]
      
      Platform.runLater {
        // Aggiorna i dadi
        updateDiceValues(battleResult.attackerDice, battleResult.defenderDice)
        
        println(s"Dadi attaccante: ${battleResult.attackerDice.mkString(", ")}")
        println(s"Dadi difensore: ${battleResult.defenderDice.mkString(", ")}")
      }
    })
  }
  
  /**
   * Configura i gestori degli eventi per i componenti UI
   */
  private def setupUIEventHandlers(): Unit = {
    actionPane.attackButton.onAction = handle {
      showAttackDialog()
    }

    actionPane.reinforceButton.onAction = handle {
      showReinforcementDialog()
    }

    actionPane.endTurnButton.onAction = handle {
      actionPane.endTurnButton.disable = true
      println("[UI] Fine turno richiesto dall'utente")
      actionHandler.endTurn(gameId).onComplete {
        case scala.util.Success(true) =>
          println("[UI] Fine turno inviato con successo al server")
        case scala.util.Success(false) =>
          Platform.runLater {
            val alert = new Alert(Alert.AlertType.Warning) {
              initOwner(GameWindow.this)
              title = "Azione non consentita"
              headerText = "Impossibile terminare il turno"
              contentText = "Il server ha rifiutato la richiesta di fine turno."
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
              contentText = s"Errore durante la richiesta di fine turno: ${ex.getMessage}"
            }
            alert.showAndWait()
            actionPane.endTurnButton.disable = false // Riabilita in caso di errore
          }
      }(networkManager.executionContext)
    }
  }

  /**
   * Mostra il dialogo per l'attacco
   */
  private def showAttackDialog(): Unit = {
    if (!attackDialogOpen) {
      try {
        // Filtra i territori per l'attacco (solo i propri con più di 1 truppa)
        val myTerritories = territories.filter(t => 
          t.owner.value == myPlayerId && t.armies.value > 1
        )
        
        // Filtra i territori nemici
        val enemyTerritories = territories.filter(t => 
          t.owner.value != myPlayerId
        )
        
        if (myTerritories.isEmpty) {
          val alert = new Alert(Alert.AlertType.Information) {
            initOwner(GameWindow.this)
            title = "Attacco non possibile"
            headerText = "Non puoi attaccare"
            contentText = "Non hai territori con abbastanza truppe per attaccare."
          }
          alert.showAndWait()
          return
        }
        
        val attackDialog = new AttackDialog(
          this, 
          myTerritories,
          enemyTerritories
        )
        
        attackDialogOpen = true
        
        attackDialog.onHidden = _ => {
          attackDialogOpen = false
        }
        
        Platform.runLater {
          attackDialog.show()
        }
      } catch {
        case e: Exception => 
          println(s"ERRORE nella creazione del dialogo di attacco: ${e.getMessage}")
          e.printStackTrace()
          attackDialogOpen = false
      }
    }
  }
  
  /**
   * Crea e visualizza la finestra di dialogo per il piazzamento delle truppe
   */
  private def showTroopPlacementDialog(myTerritories: ObservableBuffer[UITerritory], bonusTroops: Int): Unit = {
    if (!placementDialogOpen && myTerritories.nonEmpty) {
      try {
        println("Tentativo di creare e mostrare il dialogo di piazzamento...")
        val placementDialog = new TroopPlacementDialog(
          this, 
          myTerritories, 
          bonusTroops
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
      // Aggiorna solo il numero di truppe nel dialogo esistente
      currentPlacementDialog.foreach { dialog =>
        println("Aggiornamento truppe nel dialogo esistente")
        Platform.runLater {
          dialog.updateTroops(bonusTroops)
        }
      }
    }
  }
  
  /**
   * Chiude il dialogo di piazzamento truppe se aperto
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
  
  /**
   * Gestisce il messaggio GameState ricevuto dal server
   */
  private def handleGameState(gameState: GameState): Unit = {
    Platform.runLater {
      println(s"Aggiornamento stato di gioco. Fase: ${gameState.state.currentPhase}")
      
      // Aggiorna territori
      gameState.state.territories.foreach { territoryMap =>
        val name = territoryMap.getOrElse("name", "")
        val owner = territoryMap.getOrElse("owner", "")
        val troops = territoryMap.getOrElse("troops", "0").toInt
        
        updateTerritory(name, owner, troops)
      }
    
      // Aggiorna TerritoryInfoPane
      territoryInfoPane.updatePlayerTerritories(myPlayerId)
      territoryInfoPane.updateContinentControl(myPlayerId)
      
      val isMyTurn = myPlayerId == gameState.state.currentPlayer
      val currentPhase = gameState.state.currentPhase
      
      // Gestisci i controlli dell'interfaccia in base alla fase e al turno
      if (isMyTurn) {
        val myPlayerState = gameState.state.playerStates.find(_.getOrElse("playerId", "") == myPlayerId)
        
        myPlayerState.foreach { playerState =>
          val bonusTroops = playerState.getOrElse("bonusTroops", "0").toInt
          
          if (currentPhase == "SetupPhase") {
            // Nella fase di setup, mostra solo il dialogo di piazzamento
            actionPane.attackButton.disable = true
            actionPane.reinforceButton.disable = true
            actionPane.endTurnButton.disable = false
            
            val myTerritories = territories.filter(t => t.owner.value == myPlayerId)
            showTroopPlacementDialog(myTerritories, bonusTroops)
          } 
          else if (currentPhase == "MainPhase") {
            if (bonusTroops > 0) {
              // Se ci sono truppe bonus da piazzare, mostra il dialogo di piazzamento
              actionPane.attackButton.disable = true
              actionPane.reinforceButton.disable = true
              actionPane.endTurnButton.disable = true
              
              val myTerritories = territories.filter(t => t.owner.value == myPlayerId)
              showTroopPlacementDialog(myTerritories, bonusTroops)
            } 
            else {
              // Piazzamento completato, abilita tutte le azioni
              closeTroopPlacementDialog()
              actionPane.attackButton.disable = false
              // Abilita solo se hai territori validi per il rinforzo
              val canReinforce = territories.exists(t => t.owner.value == myPlayerId && t.armies.value > 1)
              actionPane.reinforceButton.disable = !canReinforce
              actionPane.endTurnButton.disable = false
            }
          }
        }
      } 
      else {
        // Non è il mio turno, disabilita tutto
        closeTroopPlacementDialog()
        actionPane.attackButton.disable = true
        actionPane.reinforceButton.disable = true
        actionPane.endTurnButton.disable = true
      }
    }
  }
  
  /**
   * Gestisce il messaggio GameStartedMessage ricevuto dal server.
   * Inizializza lo stato del gioco e mostra il dialogo di piazzamento truppe se necessario.
   */
  def handleGameStarted(gameStartedMsg: GameStartedMessage): Unit = {
    Platform.runLater {
      println("GESTIONE GAME STARTED")
      println(s"Sono: $myUsername ($myPlayerId)")
      println(s"Turno di: ${gameStartedMsg.currentPlayerId}")
      println(s"È il mio turno? ${gameStartedMsg.currentPlayerId == myPlayerId}")
      println("------------------------")
      
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
        
        // Aggiornato per utilizzare le nuove fasi
        if (myPlayerId == stateData.currentPlayer && 
           (stateData.currentPhase == "MainPhase" || stateData.currentPhase == "SetupPhase") && 
           bonusTroops > 0) {
          val myTerritories = territories.filter(t => t.owner.value == myPlayerId)
          println(s"Territori del giocatore ${myPlayerId}: ${myTerritories.size}")
          
          showTroopPlacementDialog(myTerritories, bonusTroops)
        } else if (stateData.currentPhase == "MainPhase" || stateData.currentPhase == "SetupPhase") {
          // Non è il mio turno, ma siamo in fase di piazzamento
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
      
      // Aggiorna tutto
      territoryInfoPane.updatePlayerTerritories(myPlayerId)
      territoryInfoPane.updateContinentControl(myPlayerId)
      
      println(s"Territori dopo l'aggiornamento: ${territories.size}")
      println(s"Territori del giocatore ${myPlayerId}: ${territories.count(_.owner.value == myPlayerId)}")
    }
  }
  
  /**
   * Mostra il dialogo con l'obiettivo segreto del giocatore
   */
  private def showObjectiveDialog(): Unit = {
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
  
  /**
   * Mostra il dialogo con la lista dei territori
   */
  def showTerritoriesDialog(): Unit = {
    val dialog = new TerritoriesDialog(this, territories)
    dialog.showAndWait()
  }
  
  /**
    * Crea un territorio vuoto con il nome specificato.
    */
  private def createEmptyTerritory(name: String): UITerritory = {
    val emptyTerritory = Territory(
      name = name,
      neighbors = Set.empty,
      owner = None,
      troops = 0
    )
    
    new UITerritory(emptyTerritory, "Sconosciuto")
  }

  /**
   * Restituisce l'ID della partita
   */
  def getGameId: String = gameId
  
  /**
   * Aggiorna la lista dei giocatori nella partita nella barra laterale.
   */
  def updatePlayers(newPlayers: List[String]): Unit = {
    Platform.runLater {
      playersLabel.text = s"Giocatori: ${newPlayers.size}"
      
      val updatedPlayersList = ObservableBuffer(newPlayers.map(name => 
        new PlayerInfoView(name, newPlayers, networkManager)): _*)
      
      val newSidebarPane = createSidebarPane(updatedPlayersList)
      splitPane.items.set(1, newSidebarPane)
      
      playersList.clear()
      playersList.addAll(updatedPlayersList)
      
      println(s"Lista giocatori ricreata con: ${newPlayers.mkString(", ")}")
    }
  }
  
  /**
   * Aggiorna le informazioni sui territori
   */
  def updateTerritory(name: String, owner: String, troops: Int): Unit = {
    val territory = territories.find(_.name == name)
    
   /* if (owner == myPlayerId) {
      println(s" Territorio MIO: $name ($troops truppe)")
    }*/
    
    territory match {
      case Some(t) => 
        val wasMyTerritory = t.owner.value == myPlayerId
        val becomesMyTerritory = owner == myPlayerId
        
        /* Log per i cambi di proprietà significativi
        if (!wasMyTerritory && becomesMyTerritory) {
          println(s" Ho acquisito il territorio: $name")
        } else if (wasMyTerritory && !becomesMyTerritory) {
          println(s"Ho perso il territorio: $name")
        }*/
        
        t.owner.value = owner
        t.armies.value = troops
        
      case None => 
        // Crea un nuovo territorio vuoto e aggiungilo
        val newTerritory = createEmptyTerritory(name)
        newTerritory.owner.value = owner
        newTerritory.armies.value = troops
        territories += newTerritory
        
       /* if (owner == myPlayerId) {
          println(s"Nuovo territorio MIO: $name ($troops truppe)")
        }*/
    }
  }
  
  /**
   * Aggiorna i valori dei dadi visualizzati
   */
  def updateDiceValues(attackerValues: Seq[Int], defenderValues: Seq[Int]): Unit = {
    diceDisplay.updateValues(attackerValues, defenderValues)
  }
  
  /**
   * Crea il pannello laterale della UI
   */
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
  
  /**
   * Inizializza e carica i territori
   */
  private def createTerritories(): ObservableBuffer[UITerritory] = {
    AdapterMap.loadTerritories() //resituisce una Buffer[UITerritory]
  }
  
  /**
   * Mostra il dialogo per il rinforzo delle truppe
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
}