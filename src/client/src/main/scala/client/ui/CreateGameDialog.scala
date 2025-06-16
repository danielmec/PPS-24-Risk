package client.ui

import scalafx.Includes._
import scalafx.application.Platform
import scalafx.geometry.{Insets, Pos}
import scalafx.scene.Scene
import scalafx.scene.control.{Button, Label, ListView}
import scalafx.scene.layout.{BorderPane, HBox, VBox}
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
  width = 300
  height = 200
  
  import scalafx.scene.control.{Label, TextField, ComboBox}
  
  val gameNameLabel = new Label("Nome partita:")
  val gameNameField = new TextField()
  
  val playersLabel = new Label("Numero giocatori:")
  val playersCombo = new ComboBox(ObservableBuffer(2, 3, 4, 5, 6))
  playersCombo.selectionModel().select(2)  
  
  val createButton = new Button("Crea")
  val cancelButton = new Button("Annulla")
  
  val buttonBar = new HBox(10, createButton, cancelButton)
  buttonBar.alignment = Pos.Center
  
  val layout = new VBox(10) {
    children = Seq(
      gameNameLabel, gameNameField,
      playersLabel, playersCombo, 
      buttonBar
    )
    padding = Insets(10)
    alignment = Pos.Center
  }
  
  scene = new Scene {
    root = layout
  }
  
  cancelButton.onAction = _ => close()
  
  createButton.onAction = _ => {
    val gameName = gameNameField.text.value.trim
    if (!gameName.isEmpty) {
      val maxPlayers = playersCombo.value.value
      val createMsg = ClientJsonSupport.toJson(CreateGameMessage(gameName, maxPlayers)).compactPrint
    
      networkManager.sendWebSocketMessage(createMsg).foreach { success =>
        if (success) {
          Platform.runLater(close())
        }
      }(networkManager.executionContext)
    }
  }
}


