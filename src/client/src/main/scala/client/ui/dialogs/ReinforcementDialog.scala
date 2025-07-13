package client.ui.dialogs

import scalafx.Includes._
import scalafx.application.Platform
import scalafx.geometry.Insets
import scalafx.geometry.Pos
import scalafx.scene.Scene
import scalafx.scene.control._
import scalafx.scene.layout._
import scalafx.stage.Modality
import scalafx.stage.Stage
import scalafx.collections.ObservableBuffer
import scalafx.util.StringConverter
import client.AdapterMap.UITerritory
import client.ui.GameWindow

/**
 * A modal dialog for transferring troops between two adjacent territories.
 * The user can choose a source territory, a destination territory (neighbor),
 * and the number of troops to move.
 *
 * @param owner the main game window that owns this dialog
 * @param myTerritories a list of the player's current territories
 */
class ReinforcementDialog(
  owner: GameWindow,
  myTerritories: ObservableBuffer[UITerritory]
) extends Stage {

  title = "Spostamento truppe"
  initOwner(owner)
  initModality(Modality.APPLICATION_MODAL)
  width = 400
  height = 250

  /** Converter to display UITerritory objects in combo boxes. */
  private val territoryConverter = new StringConverter[UITerritory] {
    override def toString(territory: UITerritory): String =
      if (territory == null) "" else s"${territory.name} (${territory.armies.value} truppe)"
    override def fromString(string: String): UITerritory = null
  }

  /** Combo box to select the source territory for troop movement. */
  private val fromCombo = new ComboBox[UITerritory] {
    items = ObservableBuffer.from(myTerritories.filter(_.armies.value > 1))
    promptText = "Seleziona il territorio di partenza"
    converter = territoryConverter
    onAction = _ => updateToCombo()
  }

  /** Combo box to select the destination territory (must be adjacent). */
  private val toCombo = new ComboBox[UITerritory] {
    promptText = "Seleziona il territorio di destinazione"
    disable = true
    converter = territoryConverter
    onAction = _ => updateTroopsSpinner()
  }

  /** Spinner to choose the number of troops to move. */
  private val troopsSpinner = new Spinner[Int](1, 1, 1) {
    disable = true
    editable = true
  }

  private val confirmButton = new Button("Conferma") {
    disable = true
    onAction = _ => executeReinforcement()
  }

  /** Scene and layout definition for the dialog. */
  scene = new Scene {
    root = new VBox(15) {
      padding = Insets(20)
      alignment = Pos.Center
      children = Seq(
        new Label("Sposta le truppe tra territori adiacenti") { style = "-fx-font-size: 16px; -fx-font-weight: bold;" },
        new GridPane {
          hgap = 10
          vgap = 15
          padding = Insets(10)

          add(new Label("Da:") { style = "-fx-font-weight: bold;" }, 0, 0)
          add(fromCombo, 1, 0)

          add(new Label("A:") { style = "-fx-font-weight: bold;" }, 0, 1)
          add(toCombo, 1, 1)

          add(new Label("Truppe da spostare:") { style = "-fx-font-weight: bold;" }, 0, 2)
          add(troopsSpinner, 1, 2)
        },
        new HBox(10) {
          alignment = Pos.CenterRight
          children = Seq(
            confirmButton,
            new Button("Annulla") {
              onAction = _ => close()
            }
          )
        }
      )
    }
  }

  /**
   * Updates the list of valid destination territories based on the selected source territory.
   * Only adjacent territories that are not the same as the source are included.
   * Disables destination selection if none are valid.
   */
  private def updateToCombo(): Unit = {
    Option(fromCombo.value.value).foreach { from =>
      val destinations = myTerritories.filter(t => from.isNeighbor(t) && t.name != from.name)
      toCombo.items = ObservableBuffer.from(destinations)
      toCombo.disable = destinations.isEmpty
      toCombo.selectionModel().clearSelection()
      troopsSpinner.disable = true
      confirmButton.disable = true
    }
  }

  /**
   * Updates the spinner to select the number of troops to move, based on the selected source.
   * The max value is (armies - 1) to ensure at least one troop remains.
   * Enables spinner and confirm button if both territories are selected.
   */
  private def updateTroopsSpinner(): Unit = {
    for {
      from <- Option(fromCombo.value.value)
      to <- Option(toCombo.value.value)
    } yield {
      val maxTroops = from.armies.value - 1
      troopsSpinner.valueFactory = {
        val factory = new SpinnerValueFactory.IntegerSpinnerValueFactory(1, maxTroops, 1)
        factory.asInstanceOf[SpinnerValueFactory[Int]]
      }
      troopsSpinner.disable = false
      confirmButton.disable = false
    }
  }

  /**
   * Executes the troop movement by sending the action to the game logic.
   * Updates the game state and disables the reinforce button.
   * Ends the player's turn automatically after the action.
   */
  private def executeReinforcement(): Unit = {
    for {
      from <- Option(fromCombo.value.value)
      to <- Option(toCombo.value.value)
      troops = troopsSpinner.value.value
    } yield {
      owner.actionHandler.reinforce(
        owner.getGameId,
        from.name,
        to.name,
        troops
      )
      owner.reinforcementDoneThisTurn = true
      owner.actionPane.reinforceButton.disable = true
      owner.actionHandler.endTurn(owner.getGameId)

      close()
    }
  }
}
