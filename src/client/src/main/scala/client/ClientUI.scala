package client

import scalafx.application.JFXApp3
import scalafx.application.JFXApp3.PrimaryStage
import scalafx.geometry.{Insets, Pos}
import scalafx.scene.Scene
import scalafx.scene.control.{Button, Label, TextField}
import scalafx.scene.layout.{VBox, HBox}
import scalafx.scene.text.{Font, FontWeight}
import scala.util.{Success, Failure}
import client.ClientNetworkManager
import client.ClientJsonSupport._

/**
 * Interfaccia utente semplificata per l'applicazione client Risiko
 */
object ClientUI extends JFXApp3:

  override def start(): Unit =
    
    val networkManager = new ClientNetworkManager()

    //creazione degli elementi dell'interfaccia
    val titleLabel = new Label("Risk - Client")
    titleLabel.font = Font.font("Arial", FontWeight.Bold, 20)

    val usernameLabel = new Label("Nome utente:")
    val usernameField = new TextField()
    usernameField.promptText = "Inserisci il tuo nome"
    
    val loginButton = new Button("Login")
    val statusLabel = new Label("In attesa di login...")
    statusLabel.style = "-fx-text-fill: gray;"

    
    val usernameBox = new HBox(10, usernameLabel, usernameField)
    usernameBox.alignment = Pos.Center

    
    loginButton.onAction = _ => {
      val username = usernameField.text.value.trim
      
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
            
            connectWebSocket(networkManager, statusLabel)
            
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
    mainBox.children = Seq(titleLabel, usernameBox, loginButton, statusLabel)
    mainBox.alignment = Pos.Center
    mainBox.padding = Insets(20)
    
    //configurazione della scena principale
    stage = new PrimaryStage {
      title = "Risk Client"
      scene = new Scene(400, 250) {
        root = mainBox
      }
      onCloseRequest = _ => {
        networkManager.shutdown()
      }
    }

  /**
   * Gestione della connessione WebSocket dopo il login
   */
  def connectWebSocket(networkManager: ClientNetworkManager, statusLabel: Label): Unit =
    implicit val ec = networkManager.executionContext
    
    networkManager.connectWebSocket().onComplete {
      case Success(true) =>
    
        statusLabel.text = statusLabel.text.value + " - Connesso al server"
        
        val joinLobbyMsg = ClientJsonSupport.toJson(JoinLobbyMessage()).toString
        networkManager.sendWebSocketMessage(joinLobbyMsg)
        
      case Success(false) =>
        
        statusLabel.text = statusLabel.text.value + " - Connessione al server fallita"
        statusLabel.style = "-fx-text-fill: orange;"
        
      case Failure(ex) =>
        
        statusLabel.text = statusLabel.text.value + s" - Errore connessione: ${ex.getMessage}"
        statusLabel.style = "-fx-text-fill: red;"
    }

end ClientUI