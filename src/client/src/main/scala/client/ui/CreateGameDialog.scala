package client.ui

import scalafx.Includes._
import scalafx.application.Platform
import scalafx.geometry.Insets
import scalafx.geometry.Pos
import scalafx.scene.Scene
import scalafx.scene.control.ComboBox
import scalafx.scene.control.Button
import scalafx.scene.control.Label
import scalafx.scene.control.ListView
import scalafx.scene.control.TextField
import scalafx.scene.layout.GridPane
import scalafx.scene.layout.VBox
import scalafx.scene.layout.HBox
import scalafx.scene.layout.BorderPane
import scalafx.scene.text.Font
import scalafx.scene.text.FontWeight
import scalafx.stage.Stage
import scalafx.stage.Modality
import scalafx.collections.ObservableBuffer
import client.ClientNetworkManager
import client.ClientJsonSupport._
import client.ClientJsonSupport
import client.ui.dialogs._

/**
 * A dialog for creating a new Risk game.
 *
 * This dialog allows the user to set up a new game by specifying various parameters:
 * - The player's username
 * - The name of the game session
 * - The maximum number of players allowed
 * - The number of AI bots to include
 *
 * The dialog also handles the configuration of bot players through a secondary dialog
 * if the user chooses to include bots in the game.
 *
 * @param networkManager The client network manager used to send the game creation request to the server
 */
class CreateGameDialog(networkManager: ClientNetworkManager) extends Stage {
  title = "Crea partita"
  initModality(Modality.ApplicationModal)
  width = 350
  height = 400
  
  /**
   * Text field for entering the player's username.
   * The field is pre-populated with the last used username stored in preferences.
   */
  val usernameLabel = new Label("Il tuo nome:")
  val usernameField = new TextField {
    promptText = "Inserisci il tuo nome"
    text = java.util.prefs.Preferences.userNodeForPackage(getClass)
      .get("username", "")
  }
  
  /**
   * Text field for entering the name of the game session.
   */
  val gameNameLabel = new Label("Nome partita:")
  val gameNameField = new TextField {
    promptText = "Inserisci un nome per la partita"
  }
  
  /**
   * Dropdown selector for choosing the maximum number of players.
   */
  val playersLabel = new Label("Numero giocatori:")
  val playersCombo = new ComboBox(ObservableBuffer(2, 3, 4, 5, 6))
  playersCombo.selectionModel().select(2)  
  
  /**
   * Dropdown selector for choosing the number of AI bots to include in the game.
   */
  val botsLabel = new Label("Numero di bot:")
  val botsCombo = new ComboBox(ObservableBuffer(0, 1)) {  
    selectionModel().select(0)
  }

  /**
   * Listener that updates the available bot options based on the selected number of players.
   * The maximum number of bots is always one less than the total number of players.
   */
  playersCombo.valueProperty().addListener: (_, _, newValue) =>
    val maxBots = newValue.asInstanceOf[Int] - 1
    val botOptions = ObservableBuffer((0 to maxBots).toSeq: _*)
    botsCombo.items = botOptions
    if (botsCombo.value.value > maxBots) then botsCombo.selectionModel().select(maxBots)
  
  /**
   * Grid layout containing the form fields for game configuration.
   */
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
    add(botsLabel, 0, 3)
    add(botsCombo, 1, 3)
  }
  
  /**
   * Button to confirm and send the game creation request.
   */
  val createButton = new Button("Crea")
  
  /**
   * Button to cancel the operation and close the dialog.
   */
  val cancelButton = new Button("Annulla")
  
  /**
   * Container for the action buttons.
   */
  val buttonBar = new HBox(10, createButton, cancelButton)
  buttonBar.alignment = Pos.Center
  
  /**
   * Main layout container for the dialog.
   */
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
  
  /**
   * Flag to track whether a game was successfully created.
   */
  private var gameCreated = false
  
  /**
   * Stores the last used username for reference after dialog closes.
   */
  private var lastUsedUsername = ""

  /**
   * Event handler for the create button.
   * Validates the form, configures bots if needed, and sends the game creation request.
   */
  createButton.onAction = _ => {
    val gameName = gameNameField.text.value.trim
    val username = usernameField.text.value.trim
    
    if (username.isEmpty) {
      showError("Il nome utente è obbligatorio.")
    } else if (gameName.isEmpty) {
      showError("Il nome della partita è obbligatorio.")
    } else {
      java.util.prefs.Preferences.userNodeForPackage(getClass).put("username", username)
      lastUsedUsername = username
      
      val maxPlayers = playersCombo.value.value
      val numBots = botsCombo.value.value
      
      if (numBots >= 1) 
        val botConfigDialog = new CreateBotDialog(this, numBots)
        botConfigDialog.showAndWait()
        
        if (botConfigDialog.wasConfirmed) 
          val botConfigs = botConfigDialog.getBotConfigurations
          
          val botNames = botConfigs.map(_.name).toList
          val botStrategies = botConfigs.map(_.botStrategy).toList
          
          val createMsg = CreateGameMessage(
            gameName = gameName, 
            maxPlayers = maxPlayers, 
            username = username, 
            numBots = numBots, 
            botStrategies = Some(botStrategies),
            botNames = Some(botNames)
          )
          
          sendCreateGameMessage(createMsg)
        
      else 
        val createMsg = CreateGameMessage(gameName, maxPlayers, username)
        sendCreateGameMessage(createMsg)
    }
  }

  /**
   * Sends the game creation message to the server.
   * Updates the dialog state based on the response.
   *
   * @param createMsg The CreateGameMessage to send to the server
   */
  private def sendCreateGameMessage(createMsg: CreateGameMessage): Unit = {
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

  /**
   * Indicates whether a game was successfully created through this dialog.
   *
   * @return true if a game was created, false otherwise
   */
  def wasGameCreated: Boolean = gameCreated
  
  /**
   * Returns the last username used in the dialog.
   *
   * @return the username entered by the user
   */
  def getLastUsername: String = lastUsedUsername

  /**
   * Displays an error dialog with the specified message.
   *
   * @param message The error message to display
   */
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