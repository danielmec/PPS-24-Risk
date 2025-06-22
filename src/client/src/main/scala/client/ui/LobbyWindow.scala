package client.ui

import scalafx.Includes._
import scalafx.application.Platform
import scalafx.geometry.{Insets, Pos}
import scalafx.scene.Scene
import scalafx.scene.control.{Button, Label, ListView, TextField, Dialog, ButtonType}
import scalafx.scene.layout.{BorderPane, HBox, VBox, GridPane}
import scalafx.scene.text.{Font, FontWeight}
import scalafx.stage.{Modality, Stage}
import scalafx.collections.ObservableBuffer
import client.ClientNetworkManager
import client.ClientJsonSupport._
import client.ClientJsonSupport
import java.util.prefs.Preferences

/**
 * Finestra che mostra le lobby di gioco disponibili
 */
class LobbyWindow(networkManager: ClientNetworkManager) extends Stage {
  
  // preferenze utente per salvare il nome
  private val userPrefs = Preferences.userNodeForPackage(getClass)
  
  title = "Risk - Lobby"
  initModality(Modality.ApplicationModal)
  width = 500
  height = 400
  
  // Modello dati
  val availableGames = ObservableBuffer[GameInfo]()
  
  // Componenti UI
  val titleLabel = new Label("Lobby di gioco")
  titleLabel.font = Font.font("Arial", FontWeight.Bold, 18)
  
  val gameListView = new ListView[GameInfo]()
  gameListView.items = availableGames

  
  import javafx.util.Callback
  import javafx.scene.control.{ListView => JFXListView, ListCell => JFXListCell}

  gameListView.delegate.setCellFactory(new Callback[JFXListView[GameInfo], JFXListCell[GameInfo]] {
    override def call(listView: JFXListView[GameInfo]): JFXListCell[GameInfo] = {
      new GameListCell().delegate
    }
  })
    
  
  val refreshButton = new Button("Aggiorna")
  val createGameButton = new Button("Crea partita")
  val joinGameButton = new Button("Partecipa")
  
  // inizialmente disabilitata
  joinGameButton.disable = true
  
  // Layout
  val buttonBar = new HBox(10, refreshButton, createGameButton, joinGameButton)
  buttonBar.alignment = Pos.Center
  
  val mainLayout = new BorderPane {
    top = new VBox(10, titleLabel) {
      alignment = Pos.Center
      padding = Insets(10)
    }
    center = gameListView
    bottom = new VBox(10, buttonBar) {
      alignment = Pos.Center
      padding = Insets(10)
    }
    padding = Insets(10)
  }
  
  scene = new Scene {
    root = mainLayout
  }
  
  // Gestione eventi
  gameListView.selectionModel().selectedItemProperty().addListener { (_, _, newValue) =>
    joinGameButton.disable = newValue == null
  }
  
  refreshButton.onAction = _ => {
    refreshLobby()
  }
  
  createGameButton.onAction = _ => {
    // Apre una finestra di dialogo per creare una nuova partita
    val dialog = new CreateGameDialog(networkManager)
    
    // Mostra la dialog e poi verifica se la partita è stata creata
    dialog.showAndWait()
    
    // Dopo che la dialog è stata chiusa, verifica se la partita è stata creata
    if (dialog.wasGameCreated) {
      lastEnteredUsername = Some(dialog.getLastUsername)
    }
  }
  
  // Aggiungi questo campo all'inizio della classe LobbyWindow
  private var lastEnteredUsername: Option[String] = None

  /**
   * Mostra una finestra di dialogo per richiedere il nome utente
   */
  private def showUsernameDialog(): Option[String] = {
    var result: Option[String] = None
    
    val dialog = new Stage {
      initOwner(LobbyWindow.this)
      initModality(Modality.APPLICATION_MODAL)
      title = "Inserisci username"
      width = 350
      height = 150
      
      // carica il nome precedentemente salvato
      val savedUsername = ("")
      
      val usernameLabel = new Label("Nome utente:")
      val usernameField = new TextField {
        promptText = "Inserisci il tuo nome"
        text = savedUsername
      }
      
      val okButton = new Button("Partecipa")
      val cancelButton = new Button("Annulla")
      
      val buttonBox = new HBox(10, okButton, cancelButton) {
        alignment = Pos.CenterRight
      }
      
      val formGrid = new GridPane {
        hgap = 10
        vgap = 10
        padding = Insets(10)
        
        add(usernameLabel, 0, 0)
        add(usernameField, 1, 0)
      }
      
      val layout = new VBox(20) {
        children = Seq(
          formGrid,
          buttonBox
        )
        padding = Insets(10)
      }
      
      scene = new Scene(layout)
      
      okButton.onAction = _ => {
        val username = usernameField.text.value.trim
        if (username.nonEmpty) {
          // salva nelle preferenze ma anche nel campo locale
          userPrefs.put("username", username)
          lastEnteredUsername = Some(username) // Salva localmente
          result = Some(username)
          close()
        } else {
          
          val alert = new javafx.scene.control.Alert(javafx.scene.control.Alert.AlertType.ERROR) 
          alert.setTitle("Errore")
          alert.setHeaderText("Nome utente obbligatorio")
          alert.setContentText("Inserisci un nome utente per continuare.")
          alert.showAndWait()
        }
      }
      
      cancelButton.onAction = _ => {
        result = None
        close()
      }
    }
    
    dialog.showAndWait()
    
    // restituisce il risultato direttamente
    result
  }
  
  joinGameButton.onAction = _ => {
    val selectedGame = gameListView.selectionModel().getSelectedItem
    if (selectedGame != null) {
      
      val usernameOption = showUsernameDialog()
      
      usernameOption.foreach { username =>
        println(s"Nome utente selezionato: $username")
        
        networkManager.sendMessage(JoinGameMessage(selectedGame.id, username)).foreach { success =>
          if (success) {
            println(s"Richiesta di partecipazione inviata con nome utente: $username")
          } else {
            Platform.runLater {
              showError("Non è stato possibile partecipare alla partita. Riprova più tardi.")
            }
          }
        }(networkManager.executionContext)
      }
    }
  }
  
  /**
   * Mostra un messaggio di errore in una finestra di dialogo
   * @param message Il messaggio di errore da mostrare
   */
  private def showError(message: String): Unit = {
    val alert = new javafx.scene.control.Alert(javafx.scene.control.Alert.AlertType.ERROR)
    alert.setTitle("Errore")
    alert.setHeaderText("Impossibile partecipare")
    alert.setContentText(message)
    alert.showAndWait()
  }
  
  //inizializza aggiornando la lobby
  refreshLobby()
  
  /**
   * Aggiorna la lista delle partite disponibili
   */
  private def refreshLobby(): Unit = {
    // Mostriamo un "caricamento in corso" prima di inviare la richiesta
    Platform.runLater {
      availableGames.clear()
      availableGames += GameInfo("loading", "Caricamento in corso...", 0, 0)
    }
    
    
    networkManager.sendMessage(GetAllGamesMessage()).foreach { success =>
      if (success) {
        println("Richiesta getAllGames inviata con successo")
      } else {
        Platform.runLater {
          availableGames.clear()
          availableGames += GameInfo("error", "Errore nella richiesta", 0, 0)
        }
      }
    }(networkManager.executionContext)
  }
  
  //campo per memorizzare la finestra di gioco corrente
  private var currentGameWindow: Option[GameWindow] = None

  //nel costruttore, registra i callback per i messaggi di gioco nel network manager
  networkManager.registerCallback("gameJoined", {
    case msg: GameJoinedMessage =>
      Platform.runLater {
        // Ottieni l'ultimo username inserito in questa finestra specifica
        val myUsername = lastEnteredUsername.getOrElse("")
        
        if (myUsername.nonEmpty) {
          // Verifica se questo messaggio è per me
          val isForMe = msg.players.exists(_.startsWith(s"$myUsername ("))
          
          // Verifica se sono già in questa partita
          val alreadyInThisGame = currentGameWindow.exists(_.getGameId == msg.gameId)
          
          if (isForMe && !alreadyInThisGame) {
            // Cerca il mio ID nella lista dei giocatori
            val myConnectionId = msg.players.find(_.startsWith(s"$myUsername ("))
              .map(playerString => {
                val startIdx = playerString.indexOf("(") + 1
                val endIdx = playerString.indexOf(")")
                playerString.substring(startIdx, endIdx)
              })
            
            // Log per debug
            println(s"Il mio username è: $myUsername")
            println(s"Il mio connection ID è: $myConnectionId")
            
            this.hide()
            currentGameWindow.foreach(_.close())
            
            // Passa myUsername e myConnectionId alla GameWindow
            myConnectionId.foreach { connectionId =>
              val gameWindow = new GameWindow(
                networkManager, 
                msg.gameId, 
                msg.gameName, 
                msg.players,
                myUsername,
                connectionId
              )
              
              // Verifica se c'è un messaggio gameStarted in attesa
              pendingGameStartedMsg.foreach { startedMsg =>
                println("Trovato messaggio gameStarted in attesa, processandolo...")
                Platform.runLater {
                  gameWindow.handleGameStarted(startedMsg)
                  pendingGameStartedMsg = None
                }
              }
              
              gameWindow.show()
              
              // Memorizza il riferimento alla finestra
              currentGameWindow = Some(gameWindow)
              
              // Quando la finestra di gioco viene chiusa, riapri la lobby
              gameWindow.onCloseRequest = _ => {
                this.show()
                currentGameWindow = None
              }
            }
          } else {
            println(s"Ignorato messaggio gameJoined: ${if (!isForMe) "non per me" else "già nella partita"}")
          }
        }
      }
  })

  // registra un callback per ricevere la lista dei games disponibili
  networkManager.registerCallback("gameList", {
    case msg: GameListMessage =>
      Platform.runLater {
        availableGames.clear()
        
        if (msg.games.isEmpty) {
          //nessuna partita disponibile
          availableGames += GameInfo("", "Nessuna partita disponibile", 0, 0)
        } else {
          //aggiungi le partite ricevute dal server
          msg.games.foreach { gameId =>
            availableGames += GameInfo(gameId, s"Partita: $gameId", 0, 4)
          }
        }
      }
  })

  
  networkManager.registerCallback("gameCreated", {
    case msg: GameCreatedMessage =>
      //dopo la creazione di una partita, aggiorna l'elenco
      refreshLobby()
  })
  
  // Aggiungi un campo per memorizzare temporaneamente il messaggio gameStarted
  private var pendingGameStartedMsg: Option[GameStartedMessage] = None

  // Nel costruttore di LobbyWindow, registra anche il callback per gameStarted
  networkManager.registerCallback("gameStarted", {
    case msg: GameStartedMessage =>
      Platform.runLater {
        println("Messaggio gameStarted ricevuto nella LobbyWindow")
        
        // Se c'è già una GameWindow aperta, inoltrale il messaggio
        currentGameWindow match {
          case Some(gameWindow) =>
            println("Inoltrando il messaggio alla GameWindow esistente")
            gameWindow.handleGameStarted(msg)
          
          case None =>
            // Altrimenti, salva il messaggio per passarlo alla GameWindow quando verrà creata
            println("Memorizzando il messaggio per la futura GameWindow")
            pendingGameStartedMsg = Some(msg)
        }
      }
  })

  // Classi di supporto
  
  case class GameInfo(id: String, name: String, players: Int, maxPlayers: Int) {
    override def toString: String = s"$name ($players/$maxPlayers)"
  }
  
  class GameListCell extends scalafx.scene.control.ListCell[GameInfo] {
    item.onChange { (_, _, newValue) => 
      if (newValue != null) {
        text = s"${newValue.name} (${newValue.players}/${newValue.maxPlayers})"
      } else {
        text = ""
      }
    }
  }
}

