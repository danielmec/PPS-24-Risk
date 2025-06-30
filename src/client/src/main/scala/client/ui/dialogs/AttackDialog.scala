package client.ui.dialogs

import scalafx.Includes._
import scalafx.application.Platform
import scalafx.geometry.{Insets, Pos}
import scalafx.scene.Scene
import scalafx.scene.control._
import scalafx.scene.layout._
import scalafx.stage.{Modality, Stage}
import scalafx.collections.ObservableBuffer
import client.AdapterMap.UITerritory
import client.ui.GameWindow
import client.AdapterMap
import scalafx.scene.control.SpinnerValueFactory.IntegerSpinnerValueFactory

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
  
  val fromLabel = new Label("Territorio di partenza:")
  val fromCombo = new ComboBox(ObservableBuffer("Seleziona territorio...") ++ 
    myTerritories.filter(_.armies.value > 1).map(_.name).sorted)
  fromCombo.selectionModel().selectFirst()
  val toLabel = new Label("Territorio da attaccare:")
  val toCombo = new ComboBox[String]()
  toCombo.items = ObservableBuffer("Seleziona territorio...")
  toCombo.selectionModel().selectFirst()
  toCombo.disable = true
  val troopsLabel = new Label("Truppe da usare:")
  val troopsSpinner = new Spinner[Int](1, 3, 1)
  troopsSpinner.disable = true
  
  fromCombo.onAction = _ => {
    if (fromCombo.selectionModel().selectedIndex.value > 0) {
      val selectedTerritory = myTerritories.find(_.name == fromCombo.value.value).get
      val availableTroops = selectedTerritory.armies.value - 1
      val neighborNames = selectedTerritory.neighbors
      val enemyNeighbors = enemyTerritories.filter(t => neighborNames.contains(t.name))
      
      if (enemyNeighbors.nonEmpty) {
        toCombo.items = ObservableBuffer("Seleziona territorio...") ++ 
          enemyNeighbors.map(_.name).sorted
        toCombo.disable = false
        toCombo.selectionModel().selectFirst()
      } else {
        toCombo.items = ObservableBuffer("Nessun territorio nemico confinante")
        toCombo.disable = true
        troopsSpinner.disable = true
      }
      
      troopsSpinner.valueFactory = IntegerSpinnerValueFactory(1, Math.min(3, availableTroops), 1).asInstanceOf[SpinnerValueFactory[Int]]
    } else {
      toCombo.items = ObservableBuffer("Seleziona territorio...")
      toCombo.disable = true
      troopsSpinner.disable = true
    }
  }
  
  toCombo.onAction = _ => {
    val validSelection = toCombo.selectionModel().selectedIndex.value > 0 && 
                         toCombo.value.value != "Nessun territorio nemico confinante"
    troopsSpinner.disable = !validSelection
    attackButton.disable = !validSelection
  }
  
  val attackButton = new Button("Attacca")
  attackButton.disable = true
  
  val cancelButton = new Button("Annulla")
  cancelButton.onAction = _ => close()
  
  attackButton.onAction = _ => {
    val fromTerritoryName = fromCombo.value.value
    val toTerritoryName = toCombo.value.value
    val troops = troopsSpinner.value.value
    
    val defenderId = enemyTerritories.find(_.name == toTerritoryName).get.owner.value
    
    _result = Some(AttackInfo(fromTerritoryName, toTerritoryName, troops, defenderId))
    close()
  }
  
  val formGrid = new GridPane {
    hgap = 10
    vgap = 10
    padding = Insets(10)
    
    add(fromLabel, 0, 0)
    add(fromCombo, 1, 0)
    add(toLabel, 0, 1)
    add(toCombo, 1, 1)
    add(troopsLabel, 0, 2)
    add(troopsSpinner, 1, 2)
  }
  
  val buttonBar = new HBox(10, attackButton, cancelButton) {
    alignment = Pos.Center
  }
  
  val layout = new VBox(20, formGrid, buttonBar) {
    padding = Insets(10)
  }
  
  scene = new Scene(layout)
  
  def showAndWaitWithResult(): Option[AttackInfo] = {
    super.showAndWait()
    _result
  }
}

