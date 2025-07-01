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

case class AttackInfo(
  fromTerritory: String,
  toTerritory: String,
  troops: Int,
  defenderId: String
)

class AttackDialog(
  owner: GameWindow,
  myTerritories: ObservableBuffer[UITerritory],
  enemyTerritories: ObservableBuffer[UITerritory]
) extends Stage {

  initOwner(owner)
  initModality(Modality.APPLICATION_MODAL)
  title = "Attacco"
  width = 400
  height = 300

  private var _result: Option[AttackInfo] = None

  private val territoryConverter = new StringConverter[UITerritory] {
    override def toString(t: UITerritory): String =
      if (t == null) "" else s"${t.name} (${t.armies.value} truppe)"
    override def fromString(s: String): UITerritory = null
  }

  private val fromCombo = new ComboBox[UITerritory] {
    items = ObservableBuffer.from(myTerritories.filter(_.armies.value > 1))
    promptText = "Seleziona il territorio di partenza"
    converter = territoryConverter
    onAction = _ => updateToCombo()
  }

  private val toCombo = new ComboBox[UITerritory] {
    promptText = "Seleziona il territorio da attaccare"
    disable = true
    converter = territoryConverter
    onAction = _ => updateTroopsSpinner()
  }

  private val troopsSpinner = new Spinner[Int](1, 1, 1) {
    disable = true
    editable = true
  }

  private val attackButton = new Button("Attacca") {
    disable = true
    onAction = _ => executeAttack()
  }

  private val cancelButton = new Button("Annulla") {
    onAction = _ => close()
  }

  scene = new Scene {
    root = new VBox(15) {
      padding = Insets(20)
      alignment = Pos.Center
      children = Seq(
        new Label("Attacca un territorio nemico adiacente") { style = "-fx-font-size: 16px; -fx-font-weight: bold;" },
        new GridPane {
          hgap = 10
          vgap = 15
          padding = Insets(10)

          add(new Label("Da:") { style = "-fx-font-weight: bold;" }, 0, 0)
          add(fromCombo, 1, 0)

          add(new Label("A:") { style = "-fx-font-weight: bold;" }, 0, 1)
          add(toCombo, 1, 1)

          add(new Label("Truppe da usare:") { style = "-fx-font-weight: bold;" }, 0, 2)
          add(troopsSpinner, 1, 2)
        },
        new HBox(10) {
          alignment = Pos.CenterRight
          children = Seq(attackButton, cancelButton)
        }
      )
    }
  }

  private def updateToCombo(): Unit = {
    Option(fromCombo.value.value).foreach { from =>
      val enemyNeighbors = enemyTerritories.filter(t => from.isNeighbor(t))
      toCombo.items = ObservableBuffer.from(enemyNeighbors)
      toCombo.disable = enemyNeighbors.isEmpty
      toCombo.selectionModel().clearSelection()
      troopsSpinner.disable = true
      attackButton.disable = true
    }
  }

  private def updateTroopsSpinner(): Unit = {
    for {
      from <- Option(fromCombo.value.value)
      to <- Option(toCombo.value.value)
    } yield {
      val maxTroops = math.max(1, math.min(3, from.armies.value - 1))
      troopsSpinner.valueFactory = {
        val factory = new SpinnerValueFactory.IntegerSpinnerValueFactory(1, maxTroops, 1)
        factory.asInstanceOf[SpinnerValueFactory[Int]]
      }
      troopsSpinner.disable = false
      attackButton.disable = false
    }
  }

  private def executeAttack(): Unit = {
    for {
      from <- Option(fromCombo.value.value)
      to <- Option(toCombo.value.value)
      troops = troopsSpinner.value.value
    } yield {
      val defenderId = to.owner.value
      _result = Some(AttackInfo(from.name, to.name, troops, defenderId))
      close()
    }
  }

  def showAndWaitWithResult(): Option[AttackInfo] = {
    super.showAndWait()
    _result
  }
}

