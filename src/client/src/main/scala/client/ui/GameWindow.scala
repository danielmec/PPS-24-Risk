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
  
  //Inizializzazione dei territori
  val territories = createTerritories()
  
  
  val titleLabel = new Label(s"Partita: $gameName")
  titleLabel.font = Font.font("Arial", FontWeight.Bold, 18)
  
  val playersLabel = new Label(s"Giocatori: ${initPlayers.size}")
  
  val topPane = new VBox(10) {
    padding = Insets(10)
    children = Seq(titleLabel, playersLabel)
  }
  
  //creazione componenti
  def showTerritoriesDialog(): Unit = {
    val dialog = new TerritoriesDialog(this, territories)
    dialog.showAndWait()
  }
  
  // Stampa l'ID del giocatore per debug
  println(s"GameWindow creata per il giocatore: $myUsername ($myPlayerId)")

  val gameMapView = new GameMapView(() => showTerritoriesDialog())
  private val territoryInfoPane = new TerritoryInfoPane(territories)
  val actionPane = new ActionPane(showTerritoriesDialog())
  
  
  val centerPane = new VBox {
    children = Seq(gameMapView, territoryInfoPane, actionPane)
    VBox.setVgrow(gameMapView, Priority.Always)
  }

  private var currentPlayerId: String = ""
  private var myObjective: Option[String] = None
  val actionHandler = new GameActionHandler(networkManager)(networkManager.executionContext)

  
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
  
  // Binding delle dimensioni - usando doubleValue() invece di toDouble
  gameMapView.bindToSceneDimensions(scene.width.value.doubleValue(), scene.height.value.doubleValue())
  

  /**
    * Crea un territorio vuoto con il nome specificato.
    * @param name
    * @return UITerritory vuoto con il nome specificato
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


  //callback
  networkManager.registerCallback("gameStarted", msg => {
    println(s"Callback gameStarted ricevuto!")
    handleGameStarted(msg.asInstanceOf[GameStartedMessage])
  })

  //callback 
  networkManager.registerCallback("gameState", msg => {
    println(s"Callback gameState ricevuto!")
    println(s"Sono: $myUsername ($myPlayerId)")
    
    val gameState = msg.asInstanceOf[GameState]
    println(s"Turno aggiornato: ${gameState.state.currentPlayer}")
    println(s"È il mio turno? ${gameState.state.currentPlayer == myPlayerId}")
    
    Platform.runLater {
      // Aggiorna i territori
      gameState.state.territories.foreach { territoryMap =>
        val name = territoryMap.getOrElse("name", "")
        val owner = territoryMap.getOrElse("owner", "")
        val troops = territoryMap.getOrElse("troops", "0").toInt
        
        updateTerritory(name, owner, troops)
      }
    
      //aggiorna TerritoryInfoPane
      territoryInfoPane.updatePlayerTerritories(myPlayerId)
      territoryInfoPane.updateContinentControl(myPlayerId)
      
      // cerca il mio stato giocatore
      val myPlayerState = gameState.state.playerStates.find(_.getOrElse("playerId", "") == myPlayerId)
      
      //gestisce la finestra del placement delle truppe
      myPlayerState.foreach { playerState =>
        
        val bonusTroops = playerState.getOrElse("bonusTroops", "0").toInt
        
        if (myPlayerId == gameState.state.currentPlayer && 
            gameState.state.currentPhase == "PlacingTroops" && 
            bonusTroops > 0) {
          
          // filtra solo i miei territori
          val myTerritories = territories.filter(t => t.owner.value == myPlayerId)
          
          if (myTerritories.nonEmpty) {
            try {
              println("Aprendo dialogo di piazzamento dopo gameState...")
              val placementDialog = new TroopPlacementDialog(
                GameWindow.this, 
                myTerritories, 
                bonusTroops, 
                (territory, troops) => { //passo una funzione di callback per chiamare l'azione
                  actionHandler.placeTroops(gameId, territory.name, troops)
                }
              )
              placementDialog.show()
              placementDialog.toFront()
            } catch {
              case e: Exception => 
                println(s"Errore nella creazione del dialogo (gameState): ${e.getMessage}")
                e.printStackTrace()
            }
          }
        }
      }
    }
  })

  //callback
  networkManager.registerCallback("gameJoined", msg => {
    msg match {
      case GameJoinedMessage(gameId, players, gameName) =>
        // Verifica che il messaggio sia per questa partita
        if (gameId == this.gameId) {
          Platform.runLater {
            println(s"🔄 Aggiornamento lista giocatori tramite gameJoined: ${players.mkString(", ")}")
            updatePlayers(players)
          }
        } else {
          println(s"Ignorato messaggio gameJoined per partita $gameId (sono nella ${this.gameId})")
        }
        
      case _ => println("Messaggio gameJoined ricevuto con formato non valido")
    }
  })

  //non usato
  onShown = _ => {
    println("Finestra di gioco visualizzata")
  }
  
  
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
    
    if (owner == myPlayerId) {
      println(s" Territorio MIO: $name ($troops truppe)")
    }
    
    territory match {
      case Some(t) => 
        val wasMyTerritory = t.owner.value == myPlayerId
        val becomesMyTerritory = owner == myPlayerId
        
        // Log per i cambi di proprietà significativi
        if (!wasMyTerritory && becomesMyTerritory) {
          println(s" Ho acquisito il territorio: $name")
        } else if (wasMyTerritory && !becomesMyTerritory) {
          println(s"Ho perso il territorio: $name")
        }
        
        t.owner.value = owner
        t.armies.value = troops
        
      case None => 
        // crea un nuovo territorio vuoto e aggiungilo
        val newTerritory = createEmptyTerritory(name)
        newTerritory.owner.value = owner
        newTerritory.armies.value = troops
        territories += newTerritory
        
        if (owner == myPlayerId) {
          println(s"Nuovo territorio MIO: $name ($troops truppe)")
        }
    }
  }
  
  /**
   * Aggiorna i valori dei dadi visualizzati
   */
  def updateDiceValues(attackerValues: List[Int], defenderValues: List[Int]): Unit = {
    diceDisplay.updateValues(attackerValues, defenderValues)
  }
  
  //Metodo di supporto
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
  
  private def createTerritories(): ObservableBuffer[UITerritory] = {
      AdapterMap.loadTerritories() //resituisce una Buffer[UITerritory]
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
        
        println(s"GameStarted ricevuto: currentPlayerId = $currentPlayerId")
        println(s"Il mio ID è: $myPlayerId")
        
        val initialState = gameStartedMsg.initialState
        val stateData = initialState.state
        
        println(s"Fase corrente: ${stateData.currentPhase}")
        println(s"Giocatore corrente: ${stateData.currentPlayer}")
        println(s"Territori trovati: ${stateData.territories.size}")
        println(s"Stati giocatore trovati: ${stateData.playerStates.size}")
        println(s"Esempi di territori: ${stateData.territories.take(3)}")
        
        //estrae i territori dallo stato iniziale
        stateData.territories.foreach { territoryMap =>
          val name = territoryMap.getOrElse("name", "")
          val owner = territoryMap.getOrElse("owner", "")
          val troops = territoryMap.getOrElse("troops", "0").toInt
          
          updateTerritory(name, owner, troops)
        }
        
        // cerca il mio stato giocatore (playerStates è una List[Map[String, String]])
        val myPlayerState = stateData.playerStates.find(_.getOrElse("playerId", "") == myPlayerId)
        
        println(s"Ho trovato il mio stato? ${myPlayerState.isDefined}")
        println(s"Tutti gli ID giocatore: ${stateData.playerStates.map(_.getOrElse("playerId", "")).mkString(", ")}")

        myPlayerState.foreach { playerState =>
          
              val missionCard = playerState.get("missionCard")
              println(s"Carta missione: $missionCard")
              
              val missionDesc = missionCard.flatMap { missionStr =>
                
                val pattern = "description:([^,}]+)".r
                pattern.findFirstMatchIn(missionStr.toString).map(_.group(1))

              }
              
              myObjective = missionDesc
              showObjectiveButton.disable = myObjective.isEmpty
              
              println(s"Obiettivo estratto: $missionDesc")
              println(s"Pulsante obiettivo abilitato: ${!showObjectiveButton.disable.value}")
              
              val bonusTroops = playerState.getOrElse("bonusTroops", "0").toInt
              
              println(s"Condizione 1 - È il mio turno? ${myPlayerId == stateData.currentPlayer} (io: $myPlayerId, corrente: ${stateData.currentPlayer})")
              println(s"Condizione 2 - Siamo in fase di piazzamento? ${stateData.currentPhase == "PlacingTroops"} (fase: ${stateData.currentPhase})")
              println(s"Condizione 3 - Ho truppe bonus? ${bonusTroops > 0} (truppe: $bonusTroops)")
              
              //se sono il giocatore corrente e siamo in fase di piazzamento, apri il dialog
              if (myPlayerId == stateData.currentPlayer && stateData.currentPhase == "PlacingTroops" && bonusTroops > 0) 
                {
                
                  val myTerritories = territories.filter(t => t.owner.value == myPlayerId)
                  
                  println(s"Condizione 4 - Ho territori? ${myTerritories.nonEmpty} (territori: ${myTerritories.size})")
                  
                  // Debug - stampa tutti i territori e i loro proprietari
                  territories.foreach(t => 
                    println(s"Territorio disponibile: ${t.name}, Owner: ${t.owner.value}, Match? ${t.owner.value == myPlayerId}")
                  )
                  
                  if (myTerritories.nonEmpty) {
                    try {
                      println("Tentativo di creare e mostrare il dialogo di piazzamento...")
                      val placementDialog = new TroopPlacementDialog(
                        this, 
                        myTerritories, 
                        bonusTroops, 
                        (territory, troops) => {
                          actionHandler.placeTroops(gameId, territory.name, troops)
                        }
                      )
                      
                      Platform.runLater {
                        placementDialog.show()
                        placementDialog.toFront()
                        println("Dialog visualizzato e portato in primo piano")
                      }
                    } catch {
                      case e: Exception => 
                        println(s"ERRORE nella creazione del dialogo: ${e.getMessage}")
                        e.printStackTrace()
                    }
                  } else {
                    println("Nessun territorio trovato per il piazzamento delle truppe!")
                  }
                } else if (stateData.currentPhase == "PlacingTroops") 
                  {
                      // Non è il mio turno, ma siamo in fase di piazzamento
                      Platform.runLater {
                        //ottiene il nome dell'altro giocatore
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
        
        //aggiorna tutto
        territoryInfoPane.updatePlayerTerritories(myPlayerId)
        territoryInfoPane.updateContinentControl(myPlayerId)
        
        println(s"Territori dopo l'aggiornamento: ${territories.size}")
        println(s"Territori del giocatore ${myPlayerId}: ${territories.count(_.owner.value == myPlayerId)}")

        /*territories.foreach(t => 
          println(s"Territorio: ${t.name}, Owner: ${t.owner.value}, È mio? ${t.owner.value == myPlayerId}")
        )*/

        //filtra i territori di proprietà del giocatore
        val myTerritories = territories.filter(t => t.owner.value == myPlayerId)
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
    * Metodo chiamato quando la finestra viene mostrata.
    * Registra i callback per gli eventi di gioco e aggiorna lo stato iniziale. 
   */
  onShown = _ => {
    println("Finestra di gioco visualizzata")
  }
  
  
}