package client.ui

import scalafx.application.JFXApp3
import scalafx.application.JFXApp3.PrimaryStage
import scalafx.geometry.Insets
import scalafx.geometry.Pos
import scalafx.scene.Scene
import scalafx.scene.control.TextField
import scalafx.scene.control.Label
import scalafx.scene.control.Button
import scalafx.scene.layout.HBox
import scalafx.scene.layout.VBox
import scalafx.scene.text.FontWeight
import scalafx.scene.text.Font 
import scala.util.Failure
import scala.util.Success
import client.ClientNetworkManager
import client.ClientJsonSupport
import client.ClientJsonSupport._  
import scalafx.application.Platform

/**
 * Main entry point for the Risk client application.
 * 
 * This object represents the initial UI that users interact with when launching the game.
 * It provides a simple login screen that connects to the game server and then transitions
 * to the lobby interface when successfully authenticated.
 * 
 * The UI is implemented using ScalaFX and follows a reactive pattern where server
 * communication happens asynchronously, with UI updates occurring on the JavaFX thread.
 */
object ClientUI extends JFXApp3:

  /**
   * Initializes and starts the client application UI.
   * 
   * This method sets up the initial login screen with a button to connect to the server.
   * It handles the login process and transitions to the game lobby upon successful authentication.
   * 
   * The method creates the main window components including:
   * - Title label
   * - Login button
   * - Status label for feedback
   * 
   * It also configures event handlers for user interactions and server responses.
   */
  override def start(): Unit =
    
    val networkManager = new ClientNetworkManager()

    val titleLabel = new Label("Risk - Client")
    titleLabel.font = Font.font("Arial", FontWeight.Bold, 20)

    
    val loginButton = new Button("Gioca")
    val statusLabel = new Label("In attesa di login...")
    statusLabel.style = "-fx-text-fill: gray;"

    loginButton.onAction = _ => {
      val username = "player"
      
      if (username.isEmpty) {
        statusLabel.text = "Errore: inserisci un nome utente valido"
        statusLabel.style = "-fx-text-fill: red;"
      } else {
        
        loginButton.disable = true
        statusLabel.text = s"Login in corso per '$username'..."
        statusLabel.style = "-fx-text-fill: blue;"

        implicit val ec = networkManager.executionContext
        networkManager.login(username).onComplete {
          case Success(true) =>
            val playerId = networkManager.getPlayerId.getOrElse("sconosciuto")
            statusLabel.text = s"Login effettuato! ID: $playerId"
            statusLabel.style = "-fx-text-fill: green;"
            
            connectWebSocket(networkManager, statusLabel, playerId)
            
          case Success(false) =>
            statusLabel.text = "Login fallito"
            statusLabel.style = "-fx-text-fill: red;"
            loginButton.disable = false
            
          case Failure(ex) =>
            statusLabel.text = s"Errore: ${ex.getMessage}"
            statusLabel.style = "-fx-text-fill: red;"
            loginButton.disable = false
        }
      }
    }
    val mainBox = new VBox(20)
    mainBox.children = Seq(titleLabel, loginButton, statusLabel)
    mainBox.alignment = Pos.Center
    mainBox.padding = Insets(20)
    
    stage = new PrimaryStage {
      title = "Risk Client"
      scene = new Scene(600, 350) {
        root = mainBox
      }
      onCloseRequest = _ => {
        networkManager.shutdown()
      }
    }

  /**
   * Establishes a WebSocket connection to the game server.
   *
   * This method attempts to connect to the game server via WebSocket after a successful login.
   * Upon connection, it transitions the UI from the login screen to the game lobby.
   * It handles success and failure cases, updating the status label accordingly.
   *
   * @param networkManager The network manager responsible for server communication
   * @param statusLabel The label to update with connection status information
   * @param playerId The ID of the authenticated player
   */
  def connectWebSocket(networkManager: ClientNetworkManager, statusLabel: Label, playerId: String): Unit =
    implicit val ec = networkManager.executionContext
    
    networkManager.connectWebSocket().onComplete {
      case Success(true) =>
        Platform.runLater {
          statusLabel.text = statusLabel.text.value + " - Connesso al server"
          
          val lobbyWindow = new LobbyWindow(networkManager)
          lobbyWindow.show()
          
          stage.hide()
        }
        
      case Success(false) =>
        Platform.runLater {
          statusLabel.text = statusLabel.text.value + " - Connessione al server fallita"
          statusLabel.style = "-fx-text-fill: orange;"
        }
        
      case Failure(ex) =>
        Platform.runLater {
          statusLabel.text = statusLabel.text.value + s" - Errore connessione: ${ex.getMessage}"
          statusLabel.style = "-fx-text-fill: red;"
        }
    }

end ClientUI