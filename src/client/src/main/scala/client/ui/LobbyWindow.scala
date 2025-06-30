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

class LobbyWindow(networkManager: ClientNetworkManager) extends Stage {
  
  private val userPrefs = Preferences.userNodeForPackage(getClass)
  
  title = "Risk - Lobby"
  initModality(Modality.ApplicationModal)
  width = 500
  height = 400
  
  val availableGames = ObservableBuffer[GameInfo]()
  
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
  
  joinGameButton.disable = true
  
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
  
  gameListView.selectionModel().selectedItemProperty().addListener { (_, _, newValue) =>
    joinGameButton.disable = newValue == null
  }
  
  refreshButton.onAction = _ => {
    refreshLobby()
  }
  
  createGameButton.onAction = _ => {
    val dialog = new CreateGameDialog(networkManager)
    
    dialog.showAndWait()
    
    if (dialog.wasGameCreated) {
      lastEnteredUsername = Some(dialog.getLastUsername)
    }
  }
  
  private var lastEnteredUsername: Option[String] = None

  private def showUsernameDialog(): Option[String] = {
    var result: Option[String] = None
    
    val dialog = new Stage {
      initOwner(LobbyWindow.this)
      initModality(Modality.APPLICATION_MODAL)
      title = "Inserisci username"
      width = 350
      height = 150
      
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
          userPrefs.put("username", username)
          lastEnteredUsername = Some(username) 
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
  
  private def showError(message: String): Unit = {
    val alert = new javafx.scene.control.Alert(javafx.scene.control.Alert.AlertType.ERROR)
    alert.setTitle("Errore")
    alert.setHeaderText("Impossibile partecipare")
    alert.setContentText(message)
    alert.showAndWait()
  }
  
  refreshLobby()

  private def refreshLobby(): Unit = {
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
  
  private var currentGameWindow: Option[GameWindow] = None

  networkManager.registerCallback("gameJoined", {
    case msg: GameJoinedMessage =>
      Platform.runLater {
        val myUsername = lastEnteredUsername.getOrElse("")
        
        if (myUsername.nonEmpty) {
          val isForMe = msg.players.exists(_.startsWith(s"$myUsername ("))
          
          val alreadyInThisGame = currentGameWindow.exists(_.getGameId == msg.gameId)
          
          if (isForMe && !alreadyInThisGame) {
            val myConnectionId = msg.players.find(_.startsWith(s"$myUsername ("))
              .map(playerString => {
                val startIdx = playerString.indexOf("(") + 1
                val endIdx = playerString.indexOf(")")
                playerString.substring(startIdx, endIdx)
              })
            
            println(s"Il mio username è: $myUsername")
            println(s"Il mio connection ID è: $myConnectionId")
            
            this.hide()
            currentGameWindow.foreach(_.close())
            
            myConnectionId.foreach { connectionId =>
              val gameWindow = new GameWindow(
                networkManager, 
                msg.gameId, 
                msg.gameName, 
                msg.players,
                myUsername,
                connectionId
              )

              pendingGameStartedMsg.foreach { startedMsg =>
                println("Trovato messaggio gameStarted in attesa, processandolo...")
                Platform.runLater {
                  gameWindow.handleGameStarted(startedMsg)
                  pendingGameStartedMsg = None
                }
              }
              
              gameWindow.show()
              
              currentGameWindow = Some(gameWindow)
              
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

  networkManager.registerCallback("gameList", {
    case msg: GameListMessage =>
      Platform.runLater {
        availableGames.clear()
        
        if (msg.games.isEmpty) {
          availableGames += GameInfo("", "Nessuna partita disponibile", 0, 0)
        } else {
          msg.games.foreach { gameId =>
            availableGames += GameInfo(gameId, s"Partita: $gameId", 1, 6)
          }
        }
      }
  })

  
  networkManager.registerCallback("gameCreated", {
    case msg: GameCreatedMessage =>
      refreshLobby()
  })
  
  private var pendingGameStartedMsg: Option[GameStartedMessage] = None

  networkManager.registerCallback("gameStarted", {
    case msg: GameStartedMessage =>
      Platform.runLater {
        println("Messaggio gameStarted ricevuto nella LobbyWindow")
        
        currentGameWindow match {
          case Some(gameWindow) =>
            println("Inoltrando il messaggio alla GameWindow esistente")
            gameWindow.handleGameStarted(msg)
          
          case None =>
            println("Memorizzando il messaggio per la futura GameWindow")
            pendingGameStartedMsg = Some(msg)
        }
      }
  })
  
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

