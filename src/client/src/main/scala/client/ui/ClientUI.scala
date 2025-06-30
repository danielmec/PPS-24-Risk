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

object ClientUI extends JFXApp3:

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