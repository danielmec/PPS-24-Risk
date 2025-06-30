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
import client.GameActionHandler
import client.AdapterMap
import client.AdapterMap.UITerritory
import model.board.Territory
import client.AdapterMap.UITerritory
import dialogs.{AttackDialog, ReinforcementDialog}

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
  private var currentPlacementDialog: Option[TroopPlacementDialog] = None
  var reinforcementDoneThisTurn: Boolean = false
  
  // AGGIUNGI QUESTA VARIABILE DI STATO
  private var currentPhase: String = "SetupPhase"
  private val turnIndicator = new Label("")
  turnIndicator.style = "-fx-font-size: 14px; -fx-font-weight: bold; -fx-padding: 5px;"
  
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
    children = Seq(titleLabel, playerInfoLabel, turnIndicator, playersLabel)
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
      // AGGIORNA LA FASE
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
  }
  
  /**
   * Configura i gestori degli eventi per i componenti UI
   */
  private def setupUIEventHandlers(): Unit = {
    actionPane.endTurnButton.onAction = handle {
      actionPane.endTurnButton.disable = true
      
      // USA LA VARIABILE DI STATO LOCALE
      println(s"[UI] Fine turno richiesto dall'utente (fase corrente: $currentPhase)")
      
      // Scegli l'azione corretta in base alla fase
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
            actionPane.endTurnButton.disable = false // Riabilita in caso di errore
          }
      }(networkManager.executionContext)
    }
    
    // Nuovo codice per il pulsante di attacco
    actionPane.attackButton.onAction = handle {
      val myTerritories = territories.filter(_.owner.value == myPlayerId)
      if (myTerritories.isEmpty) {
        showErrorAlert("Non hai territori da cui attaccare")
        return
      }
      
      val attackDialog = new AttackDialog(
        this,
        myTerritories,
        territories.filter(_.owner.value != myPlayerId)
      )
      
      val result = attackDialog.showAndWaitWithResult() // Usa il nuovo metodo
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


    // Nuovo codice per il pulsante di rinforzo
    actionPane.reinforceButton.onAction = handle {
      showReinforcementDialog()
    }

    
  }
  
  // Metodo di utilità per mostrare errori
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
          bonusTroops,
          currentPhase  // Passa la fase corrente
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
      
      // Trova il nome completo del giocatore corrente
      val currentPlayerName = gameState.players.find(p => p.contains(gameState.state.currentPlayer))
        .map(p => p.split(" \\(").head)
        .getOrElse("Sconosciuto")
      
      // Aggiorna l'indicatore di turno
      if (isMyTurn) {
        turnIndicator.text = s"È IL TUO TURNO - Fase: $currentPhase"
        turnIndicator.style = "-fx-text-fill: green; -fx-font-size: 14px; -fx-font-weight: bold; -fx-padding: 5px; -fx-background-color: #e8ffe8; -fx-border-color: green; -fx-border-width: 1px; -fx-border-radius: 3px;"
      } else {
        turnIndicator.text = s"Turno di $currentPlayerName - Fase: $currentPhase"
        turnIndicator.style = "-fx-text-fill: #555; -fx-font-size: 14px; -fx-font-weight: bold; -fx-padding: 5px; -fx-background-color: #f8f8f8; -fx-border-color: #aaa; -fx-border-width: 1px; -fx-border-radius: 3px;"
      }
      
      // Gestisci i controlli dell'interfaccia in base alla fase e al turno
      if (isMyTurn) {
        val myPlayerState = gameState.state.playerStates.find(_.getOrElse("playerId", "") == myPlayerId)
        
        myPlayerState.foreach { playerState =>
          val bonusTroops = playerState.getOrElse("bonusTroops", "0").toInt
          
          if (currentPhase == "SetupPhase") {
            // Nella fase di setup, mostra solo il dialogo di piazzamento
            actionPane.getAttackButton.disable = true
            actionPane.getReinforceButton.disable = true
            actionPane.getEndTurnButton.disable = false
            
            val myTerritories = territories.filter(t => t.owner.value == myPlayerId)
            showTroopPlacementDialog(myTerritories, bonusTroops)
          } 
          else if (currentPhase == "MainPhase") {
            if (bonusTroops > 0 && gameState.state.playerStartedTurn == "true") {
              // Se ci sono truppe bonus da piazzare E siamo all'inizio del turno, mostra il dialogo di piazzamento
              actionPane.getAttackButton.disable = true
              actionPane.getReinforceButton.disable = true
              actionPane.getEndTurnButton.disable = true
              
              println(s"[handleGameState] Mostro dialogo piazzamento - Inizio turno: ${gameState.state.playerStartedTurn}, bonus: $bonusTroops")
              val myTerritories = territories.filter(t => t.owner.value == myPlayerId)
              showTroopPlacementDialog(myTerritories, bonusTroops)
            } 
            else {
              // Piazzamento completato, abilita tutte le azioni
              closeTroopPlacementDialog()
              actionPane.getAttackButton.disable = false
              actionPane.getReinforceButton.disable = false
              actionPane.getEndTurnButton.disable = false
            }
          }
        }
      } 
      else {
        // Non è il mio turno, disabilita tutto
        closeTroopPlacementDialog()
        actionPane.getAttackButton.disable = true
        actionPane.getReinforceButton.disable = true
        actionPane.getEndTurnButton.disable = true
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
        
        // CORREZIONE: Mostra il dialogo di piazzamento truppe solo all'inizio del turno del giocatore
        if (myPlayerId == stateData.currentPlayer && 
           (stateData.currentPhase == "MainPhase" || stateData.currentPhase == "SetupPhase") && 
           bonusTroops > 0) {
           
          println(s"[handleGameState] CONTROLLO INIZIO TURNO - playerStartedTurn: '${stateData.playerStartedTurn}'")
          
          if (stateData.playerStartedTurn == "true") {  // Aggiungiamo il controllo sul flag playerStartedTurn
            val myTerritories = territories.filter(t => t.owner.value == myPlayerId)
            println(s"Territori del giocatore ${myPlayerId}: ${myTerritories.size}")
            println(s"Inizio turno: ${stateData.playerStartedTurn} - Mostrando dialogo piazzamento truppe")
            
            showTroopPlacementDialog(myTerritories, bonusTroops)
          } else {
            println(s"[handleGameState] NON mostro dialogo perché playerStartedTurn = '${stateData.playerStartedTurn}'")
          }
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
  def updateDiceValues(attackerValues: List[Int], defenderValues: List[Int]): Unit = {
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
   * Gestisce la visualizzazione del risultato di una battaglia
   */
  private def handleBattleResult(battleResult: BattleResultMessage): Unit = {
    println(s"[UI] Ricevuto risultato battaglia: ${battleResult}")

    Platform.runLater {
      try {
        // Verifica che il componente diceDisplay sia nella scena
        println(s"[UI] diceDisplay: visibile=${diceDisplay.visible.value}, width=${diceDisplay.width.value}, height=${diceDisplay.height.value}")
        
        // Forza la visibilità del componente
        diceDisplay.visible = true
        diceDisplay.opacity = 1.0
        diceDisplay.managed = true
        
        // Aggiorna il display dei dadi
        println(s"[UI] Aggiorno i dadi con: Attaccante=${battleResult.attackerDice.mkString(",")}, Difensore=${battleResult.defenderDice.mkString(",")}")
        diceDisplay.updateValues(battleResult.attackerDice, battleResult.defenderDice)
        
        // Crea un flash effect per attirare l'attenzione sull'area dei dadi
        val originalStyle = diceDisplay.style.value
        diceDisplay.style = originalStyle + "-fx-background-color: #ffcccc;"
        
        // Ripristina lo stile originale dopo un breve intervallo
        val timer = new java.util.Timer()
        timer.schedule(new java.util.TimerTask {
          override def run(): Unit = {
            Platform.runLater {
              diceDisplay.style = originalStyle
            }
          }
        }, 500)
        
        // Mostra un alert con il risultato della battaglia
        val outcomeText = if (battleResult.conquered) 
          s"Territorio ${battleResult.defenderTerritory} conquistato!" 
        else 
          s"Attacco a ${battleResult.defenderTerritory} respinto."
        
        // Mostra un messaggio di notifica solo se non è un'azione del giocatore corrente
        if (currentPlayerId != myPlayerId) {
          val attackMessage = s"Attacco da ${battleResult.attackerTerritory} a ${battleResult.defenderTerritory}"
          val lossesMessage = s"$outcomeText\nPerdite: Attaccante ${battleResult.attackerLosses}, Difensore ${battleResult.defenderLosses}"
          
          val alert = new Alert(Alert.AlertType.Information) {
            initOwner(GameWindow.this)
            title = "Risultato Battaglia"
            headerText = attackMessage
            contentText = lossesMessage
          }
          alert.show()
        }
      } catch {
        case ex: Exception =>
          println(s"[UI] ERRORE durante la gestione del risultato battaglia: ${ex.getMessage}")
          ex.printStackTrace()
      }
    }
  }
  
  /**
   * Gestisce la visualizzazione di uno spostamento di truppe
   */
  private def handleTroopMovement(movement: TroopMovementMessage): Unit = {
    Platform.runLater {
      // Se il movimento è fatto da un altro giocatore, mostra una notifica
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
  
  /**
   * Gestisce il messaggio di fine gioco mostrando un dialogo informativo
   */
  private def handleGameOver(gameOver: GameOverMessage): Unit = {
    Platform.runLater {
      // Determina se il giocatore corrente è il vincitore
      val winnerIsCurrentPlayer = gameOver.winnerId == myPlayerId
      
      // Crea un dialogo personalizzato in base al risultato
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
        
        // Personalizza lo stile del dialogo
        dialogPane().style = "-fx-background-color: #f0f8ff;"
      }
      
      // Mostra il dialogo e poi chiude la finestra di gioco
      alert.showAndWait()
      
      close()
    }
  }

}