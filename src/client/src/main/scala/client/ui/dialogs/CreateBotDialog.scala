package client.ui.dialogs

import scalafx.Includes._
import scalafx.geometry.Insets
import scalafx.geometry.Pos
import scalafx.scene.Scene
import scalafx.scene.control.TextField
import scalafx.scene.control.Button 
import scalafx.scene.control.Label 
import scalafx.scene.control.ComboBox 
import scalafx.scene.layout.VBox
import scalafx.scene.layout.GridPane
import scalafx.scene.layout.HBox
import scalafx.scene.text.FontWeight
import scalafx.scene.text.Font 
import scalafx.stage.Modality
import scalafx.stage.Stage
import scalafx.collections.ObservableBuffer

/**
 * A modal dialog for creating and configuring bots.
 * Each bot has a name and a selected strategy (Offensive or Defensive).
 *
 * @param parentStage the parent window that owns this dialog
 * @param numBots the number of bots to configure
 */
class CreateBotDialog(parentStage: Stage, numBots: Int) extends Stage {

  title = "Crea Bot"
  initModality(Modality.APPLICATION_MODAL)
  initOwner(parentStage)
  width = 400
  height = 200 + numBots * 40

  /** Available bot strategies. */
  val botStrategies = ObservableBuffer("Offensivo", "Difensivo")

  /**
   * Internal structure to hold UI components for a single bot configuration:
   * a name field and a strategy combo box.
   */
  case class BotConfig(strategySelector: ComboBox[String], nameField: TextField)

  /** List of bot configuration UI components. */
  val botConfigs = Array.tabulate(numBots) { i =>
    val comboBox = new ComboBox(botStrategies)
    comboBox.selectionModel().selectFirst()
    val nameField = new TextField {
      text = s"Bot ${i + 1}"
      prefWidth = 150
    }
    BotConfig(comboBox, nameField)
  }

  /** Grid layout containing the name and strategy inputs for all bots. */
  val grid = new GridPane:
    hgap = 10
    vgap = 10
    padding = Insets(20)
    add(new Label("Nome") { style = "-fx-font-weight: bold" }, 0, 0)
    add(new Label("Strategia") { style = "-fx-font-weight: bold" }, 1, 0)
    for (i <- 0 until numBots)
      add(botConfigs(i).nameField, 0, i + 1)
      add(botConfigs(i).strategySelector, 1, i + 1)

  /** Button to confirm bot creation. */
  val confirmButton = new Button("Conferma")

  /** Button to cancel the dialog without saving changes. */
  val cancelButton = new Button("Annulla")

  /** Container for action buttons. */
  val buttonBar = new HBox(10, confirmButton, cancelButton) {
    alignment = Pos.Center
  }

  /** Main layout container for the dialog. */
  val layout = new VBox(20) {
    children = Seq(
      new Label("Crea i bot") {
        font = Font.font("Arial", FontWeight.Bold, 14)
        alignmentInParent = Pos.Center
      },
      grid,
      buttonBar
    )
    padding = Insets(20)
    alignment = Pos.Center
  }

  scene = new Scene {
    root = layout
  }

  /** Flag indicating whether the user confirmed the dialog. */
  private var confirmed = false

  /**
   * Final bot configuration: name and selected strategy.
   *
   * @param name the bot name
   * @param botStrategy the selected strategy ("Offensive" or "Defensive")
   */
  case class BotConfiguration(name: String, botStrategy: String)

  /**
   * Retrieves the configuration for each bot entered by the user.
   * If the name field is empty, a default name like "Bot 1" is used.
   *
   * @return an array of bot configurations
   */
  def getBotConfigurations: Array[BotConfiguration] =
    botConfigs.map(config => BotConfiguration(
      name = config.nameField.text.value.trim.nonEmpty match {
        case true  => config.nameField.text.value.trim
        case false => s"Bot ${botConfigs.indexOf(config) + 1}"
      },
      botStrategy = config.strategySelector.value.value
    ))

  /**
   * Indicates whether the user confirmed the dialog.
   *
   * @return true if the "Confirm" button was clicked, false otherwise
   */
  def wasConfirmed: Boolean = confirmed

  // Event handlers for dialog buttons
  confirmButton.onAction = _ =>
    confirmed = true
    close()

  cancelButton.onAction = _ => close()
}
