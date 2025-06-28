package client.ui.dialogs

import scalafx.Includes._
import scalafx.application.Platform
import scalafx.geometry.{Insets, Pos}
import scalafx.scene.Scene
import scalafx.scene.control._
import scalafx.scene.layout._
import scalafx.stage.{Modality, Stage}
import scalafx.collections.ObservableBuffer
import scalafx.util.StringConverter
import client.AdapterMap.UITerritory
import client.ui.GameWindow

class ReinforcementDialog(
  owner: GameWindow,
  myTerritories: ObservableBuffer[UITerritory]
) extends Stage {

  title = "Spostamento truppe"
  initOwner(owner)
  initModality(Modality.APPLICATION_MODAL)
  width = 400
  height = 250

  private val territoryConverter = new StringConverter[UITerritory] {
    override def toString(territory: UITerritory): String =
      if (territory == null) "" else s"${territory.name} (${territory.armies.value} truppe)"
    override def fromString(string: String): UITerritory = null
  }

  private val fromCombo = new ComboBox[UITerritory] {
    items = ObservableBuffer.from(myTerritories.filter(_.armies.value > 1))
    promptText = "Seleziona il territorio di partenza"
    converter = territoryConverter
    onAction = _ => updateToCombo()
  }

  private val toCombo = new ComboBox[UITerritory] {
    promptText = "Seleziona il territorio di destinazione"
    disable = true
    converter = territoryConverter
    onAction = _ => updateTroopsSpinner()
  }

  private val troopsSpinner = new Spinner[Int](1, 1, 1) {
    disable = true
    editable = true
  }

  private val confirmButton = new Button("Conferma") {
    disable = true
    onAction = _ => executeReinforcement()
  }

  // Layout
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

  private def updateToCombo(): Unit = {
    Option(fromCombo.value.value).foreach { from =>
      val destinazioni = myTerritories.filter(t => from.isNeighbor(t) && t.name != from.name)
      toCombo.items = ObservableBuffer.from(destinazioni)
      toCombo.disable = destinazioni.isEmpty
      toCombo.selectionModel().clearSelection()
      troopsSpinner.disable = true
      confirmButton.disable = true
    }
  }

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
      close()
    }
  }
}


