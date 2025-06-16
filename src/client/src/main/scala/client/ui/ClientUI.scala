package client.ui

import scalafx.application.JFXApp3
import scalafx.application.JFXApp3.PrimaryStage
import scalafx.geometry.{Insets, Pos}
import scalafx.scene.Scene
import scalafx.scene.control.{Button, Label, TextField}
import scalafx.scene.layout.{VBox, HBox}
import scalafx.scene.text.{Font, FontWeight}
import scala.util.{Success, Failure}
import client.ClientNetworkManager
import client.ClientJsonSupport
import client.ClientJsonSupport._  
import scalafx.application.Platform

/**
 * Interfaccia utente semplificata per l'applicazione client Risiko
 */
object ClientUI extends JFXApp3:

  override def start(): Unit =
    
    val networkManager = new ClientNetworkManager()

    val titleLabel = new Label("Risk - Client")
    titleLabel.font = Font.font("Arial", FontWeight.Bold, 20)

    
    val loginButton = new Button("Entra in Modalità Multigiocatore")
    val singlePlayerButton = new Button("Entra in Modalità Singolo")
    val statusLabel = new Label("In attesa di login...")
    statusLabel.style = "-fx-text-fill: gray;"


    singlePlayerButton.onAction = _ => {
      
      statusLabel.text = "Modalità singolo non ancora implementata"
      statusLabel.style = "-fx-text-fill: orange;"
        
    }

    loginButton.onAction = _ => {
      val username = "player"
      
      if (username.isEmpty) {
        statusLabel.text = "Errore: inserisci un nome utente valido"
        statusLabel.style = "-fx-text-fill: red;"
      } else {
        
        loginButton.disable = true
        statusLabel.text = s"Login in corso per '$username'..."
        statusLabel.style = "-fx-text-fill: blue;"
        
        //esegue il login in modo asincrono
        implicit val ec = networkManager.executionContext
        networkManager.login(username).onComplete {
          case Success(true) =>
            val playerId = networkManager.getPlayerId.getOrElse("sconosciuto")
            statusLabel.text = s"Login effettuato! ID: $playerId"
            statusLabel.style = "-fx-text-fill: green;"
            
            //dopo il login, connette col WebSocket
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
    
    //creazione del layout principale
    val mainBox = new VBox(20)
    mainBox.children = Seq(titleLabel, loginButton, singlePlayerButton, statusLabel)
    mainBox.alignment = Pos.Center
    mainBox.padding = Insets(20)
    
    //configurazione della scena principale
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
   * Gestione della connessione WebSocket dopo il login
   */
  def connectWebSocket(networkManager: ClientNetworkManager, statusLabel: Label, playerId: String): Unit =
    implicit val ec = networkManager.executionContext
    
    networkManager.connectWebSocket().onComplete {
      case Success(true) =>
        Platform.runLater {
          statusLabel.text = statusLabel.text.value + " - Connesso al server"
          
          // una volta connessi, si apre la finestra delle lobby
          val lobbyWindow = new LobbyWindow(networkManager, playerId)
          lobbyWindow.show()
          
          // nasconde la finestra di login
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