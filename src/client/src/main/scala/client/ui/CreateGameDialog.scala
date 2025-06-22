package client.ui

import scalafx.Includes._
import scalafx.application.Platform
import scalafx.geometry.{Insets, Pos}
import scalafx.scene.Scene
import scalafx.scene.control.{Button, Label, ListView, TextField, ComboBox}
import scalafx.scene.layout.{BorderPane, HBox, VBox, GridPane}
import scalafx.scene.text.{Font, FontWeight}
import scalafx.stage.{Modality, Stage}
import scalafx.collections.ObservableBuffer
import client.ClientNetworkManager
import client.ClientJsonSupport._
import client.ClientJsonSupport

/**
 * Finestra di dialogo per la creazione di una nuova partita
 */
class CreateGameDialog(networkManager: ClientNetworkManager) extends Stage {
  title = "Crea partita"
  initModality(Modality.ApplicationModal)
  width = 350
  height = 250
  
  // Aggiungi un campo per il nome utente
  val usernameLabel = new Label("Il tuo nome:")
  val usernameField = new TextField {
    promptText = "Inserisci il tuo nome"
    // Prova a caricare il nome utente salvato nelle preferenze
    text = java.util.prefs.Preferences.userNodeForPackage(getClass)
      .get("username", "")
  }
  
  val gameNameLabel = new Label("Nome partita:")
  val gameNameField = new TextField {
    promptText = "Inserisci un nome per la partita"
  }
  
  val playersLabel = new Label("Numero giocatori:")
  val playersCombo = new ComboBox(ObservableBuffer(2, 3, 4, 5, 6))
  playersCombo.selectionModel().select(2)  
  
  // Usa un GridPane per allineare meglio i campi
  val formGrid = new GridPane {
    hgap = 10
    vgap = 10
    padding = Insets(10)
    
    add(usernameLabel, 0, 0)
    add(usernameField, 1, 0)
    add(gameNameLabel, 0, 1)
    add(gameNameField, 1, 1)
    add(playersLabel, 0, 2)
    add(playersCombo, 1, 2)
  }
  
  val createButton = new Button("Crea")
  val cancelButton = new Button("Annulla")
  
  val buttonBar = new HBox(10, createButton, cancelButton)
  buttonBar.alignment = Pos.Center
  
  val layout = new VBox(20) {
    children = Seq(
      new Label("Crea una nuova partita") {
        font = Font.font("Arial", FontWeight.Bold, 14)
        alignmentInParent = Pos.Center
      },
      formGrid,
      buttonBar
    )
    padding = Insets(10)
    alignment = Pos.Center
  }
  
  scene = new Scene {
    root = layout
  }
  
  cancelButton.onAction = _ => close()
  
  // Aggiungi questi campi per tenere traccia dello stato
  private var gameCreated = false
  private var lastUsedUsername = ""

  createButton.onAction = _ => {
    val gameName = gameNameField.text.value.trim
    val username = usernameField.text.value.trim
    
    if (username.isEmpty) {
      showError("Il nome utente è obbligatorio.")
    } else if (gameName.isEmpty) {
      showError("Il nome della partita è obbligatorio.")
    } else {
      // Salva il nome utente nelle preferenze per usi futuri
      java.util.prefs.Preferences.userNodeForPackage(getClass)
        .put("username", username)
      
      // Salva l'username usato per questa creazione
      lastUsedUsername = username
      
      val maxPlayers = playersCombo.value.value
      val createMsg = CreateGameMessage(gameName, maxPlayers, username)
      
      networkManager.sendMessage(createMsg).onComplete {
        case scala.util.Success(_) => Platform.runLater {
          gameCreated = true
          close()
        }
        case scala.util.Failure(e) => Platform.runLater {
          showError(s"Errore: ${e.getMessage}")
        }
      }(networkManager.executionContext)
    }
  }

  // Aggiungi questi metodi per accedere ai valori
  def wasGameCreated: Boolean = gameCreated
  def getLastUsername: String = lastUsedUsername

  // Metodo di utilità per mostrare un messaggio di errore
  private def showError(message: String): Unit = {
    import scalafx.scene.control.Alert
    import scalafx.scene.control.Alert.AlertType
    
    new Alert(AlertType.Error) {
      title = "Errore"
      headerText = "Impossibile creare la partita"
      contentText = message
    }.showAndWait()
  }
}


