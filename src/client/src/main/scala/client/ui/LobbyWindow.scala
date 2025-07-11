package client.ui

import scalafx.Includes._
import scalafx.application.Platform
import scalafx.geometry.Insets
import scalafx.geometry.Pos
import scalafx.scene.Scene
import scalafx.scene.control.ButtonType
import scalafx.scene.control.Button 
import scalafx.scene.control.Label 
import scalafx.scene.control.ListView 
import scalafx.scene.control.TextField 
import scalafx.scene.control.Dialog 
import scalafx.scene.layout.GridPane
import scalafx.scene.layout.BorderPane
import scalafx.scene.layout.HBox
import scalafx.scene.layout.VBox
import scalafx.scene.text.FontWeight
import scalafx.scene.text.Font 
import scalafx.stage.Modality
import scalafx.stage.Stage
import scalafx.collections.ObservableBuffer
import client.ClientNetworkManager
import client.ClientJsonSupport._
import client.ClientJsonSupport
import java.util.prefs.Preferences
import javafx.util.Callback
import javafx.scene.control.{ListView => JFXListView, ListCell => JFXListCell}

/**
 * Main window displaying available games and allowing users to create or join games.
 * 
 * @param networkManager Client network manager for server communication
 */
class LobbyWindow(networkManager: ClientNetworkManager) extends Stage {
  
  private val userPrefs = Preferences.userNodeForPackage(getClass)
  private var lastEnteredUsername: Option[String] = None
  private var currentGameWindow: Option[GameWindow] = None
  private var pendingGameStartedMsg: Option[GameStartedMessage] = None
  
  title = "Risk - Lobby"
  initModality(Modality.ApplicationModal)
  width = 500
  height = 400
  
  private val availableGames = ObservableBuffer[GameInfo]()
  
  private val titleLabel = new Label("Lobby di gioco") {
    font = Font.font("Arial", FontWeight.Bold, 18)
  }
  
  private val gameListView = new ListView[GameInfo]() {
    items = availableGames
  }

  gameListView.delegate.setCellFactory(new Callback[JFXListView[GameInfo], JFXListCell[GameInfo]] {
    override def call(listView: JFXListView[GameInfo]): JFXListCell[GameInfo] = {
      new GameListCell().delegate
    }
  })
    
  private val refreshButton = new Button("Aggiorna")
  private val createGameButton = new Button("Crea partita")
  private val joinGameButton = new Button("Partecipa") {
    disable = true
  }
  
  private val buttonBar = new HBox(10, refreshButton, createGameButton, joinGameButton) {
    alignment = Pos.Center
  }
  
  private val mainLayout = new BorderPane {
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
  
  refreshButton.onAction = _ => refreshLobby()
  
  createGameButton.onAction = _ => {
    val dialog = new CreateGameDialog(networkManager)
    dialog.showAndWait()
    
    if (dialog.wasGameCreated) {
      lastEnteredUsername = Some(dialog.getLastUsername)
    }
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

  refreshLobby()
  registerNetworkCallbacks()


  /**
   * Shows a dialog to enter a username
   *
   * @return Option containing the entered username or None if canceled
   */
  private def showUsernameDialog(): Option[String] = {
    var result: Option[String] = None
    
    val dialog = new Stage {
      initOwner(LobbyWindow.this)
      initModality(Modality.APPLICATION_MODAL)
      title = "Inserisci username"
      width = 350
      height = 150
      
      val savedUsername = ""
      
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
  
  /**
   * Shows an error message to the user
   *
   * @param message The message to display
   */
  private def showError(message: String): Unit = {
    val alert = new javafx.scene.control.Alert(javafx.scene.control.Alert.AlertType.ERROR)
    alert.setTitle("Errore")
    alert.setHeaderText("Impossibile partecipare")
    alert.setContentText(message)
    alert.showAndWait()
  }
  
  /**
   * Refreshes the list of available games by requesting it from the server
   */
  private def refreshLobby(): Unit = {
    Platform.runLater {
      availableGames.clear()
      availableGames += GameInfo("loading", "Caricamento in corso...", 0, 0)
    }
    
    networkManager.sendMessage(GetAllGamesMessage()).foreach { success =>
      if (!success) {
        Platform.runLater {
          availableGames.clear()
          availableGames += GameInfo("error", "Errore nella richiesta", 0, 0)
        }
      }
    }(networkManager.executionContext)
  }
  
  /**
   * Registers all callbacks for handling network messages
   */
  private def registerNetworkCallbacks(): Unit = {
    networkManager.registerCallback("gameList", {
      case msg: GameListMessage =>
        Platform.runLater {
          availableGames.clear()
          
          if (msg.games.isEmpty) {
            availableGames += GameInfo("", "Nessuna partita disponibile", 0, 0)
          } else {
            msg.games.foreach { case (gameId, gameName) =>
              availableGames += GameInfo(gameId, gameName, 1, 6)
            }
          }
        }
    })
    
    networkManager.registerCallback("gameCreated", {
      case _: GameCreatedMessage =>
        refreshLobby()
    })
    
    networkManager.registerCallback("gameJoined", {
      case msg: GameJoinedMessage =>
        Platform.runLater {
          handleGameJoined(msg)
        }
    })
    
    networkManager.registerCallback("gameStarted", {
      case msg: GameStartedMessage =>
        Platform.runLater {
          currentGameWindow match {
            case Some(gameWindow) =>
              gameWindow.handleGameStarted(msg)
            case None =>
              pendingGameStartedMsg = Some(msg)
          }
        }
    })
  }
  
  /**
   * Handles the response to joining a game by creating the game window
   * 
   * @param msg The game joined message received
   */
  private def handleGameJoined(msg: GameJoinedMessage): Unit = {
    val myUsername = lastEnteredUsername.getOrElse("")
    
    if (myUsername.nonEmpty) {
      val isForMe = msg.players.exists(_.startsWith(s"$myUsername ("))
      val alreadyInThisGame = currentGameWindow.exists(_.getGameId == msg.gameId)
      
      if (isForMe && !alreadyInThisGame) {
        val myConnectionId = extractConnectionId(myUsername, msg.players)
        
        this.hide()
        currentGameWindow.foreach(_.close())
        
        myConnectionId.foreach { connectionId =>
          createGameWindow(
            msg.gameId, 
            msg.gameName, 
            msg.players,
            myUsername,
            connectionId,
            msg.playerColors.getOrElse(Map.empty)
          )
        }
      }
    }
  }
  
  /**
   * Extracts the connection ID from a string in the format "username (connectionId)"
   *
   * @param username The username to look for
   * @param players The list of players with their connection IDs
   * @return The connection ID if found
   */
  private def extractConnectionId(username: String, players: List[String]): Option[String] = {
    players.find(_.startsWith(s"$username ("))
      .map { playerString =>
        val startIdx = playerString.indexOf("(") + 1
        val endIdx = playerString.indexOf(")")
        playerString.substring(startIdx, endIdx)
      }
  }
  
  /**
   * Creates and shows the game window
   */
  private def createGameWindow(
    gameId: String, 
    gameName: String, 
    players: List[String],
    myUsername: String,
    connectionId: String,
    playerColors: Map[String, String]
  ): Unit = {
    val gameWindow = new GameWindow(
      networkManager, 
      gameId, 
      gameName, 
      players,
      myUsername,
      connectionId,
      playerColors
    )

    pendingGameStartedMsg.foreach { startedMsg =>
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
  
  // ===================================
  // Inner classes
  // ===================================
  
  /**
   * Represents the basic information of a game to display in the lobby
   * 
   * @param id Unique game identifier
   * @param name Display name of the game
   * @param players Number of currently connected players
   * @param maxPlayers Maximum number of players supported
   */
  case class GameInfo(id: String, name: String, players: Int, maxPlayers: Int) {
    override def toString: String = s"$name ($players/$maxPlayers)"
  }
  
  /**
   * Custom cell for displaying game information in the list
   * Shows name, number of players, and game ID
   */
  class GameListCell extends scalafx.scene.control.ListCell[GameInfo] {
    item.onChange { (_, _, newValue) => 
      if (newValue != null) {
        text = s"${newValue.name} [ID: ${newValue.id}]"
      } else {
        text = ""
      }
    }
  }
}

