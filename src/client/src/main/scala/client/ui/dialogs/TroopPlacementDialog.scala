package client.ui.dialogs

import scalafx.Includes._
import scalafx.application.Platform
import scalafx.geometry.Insets
import scalafx.geometry.Pos
import scalafx.scene.Scene
import scalafx.scene.control._
import scalafx.scene.layout._
import scalafx.stage.Stage
import scalafx.stage.Modality
import scalafx.stage.StageStyle
import scalafx.collections.ObservableBuffer
import scalafx.beans.property.ObjectProperty
import scalafx.beans.property.IntegerProperty
import scalafx.beans.binding.Bindings
import client.AdapterMap.UITerritory
import client.ui.GameWindow


class TroopPlacementDialog(
  owner: GameWindow,
  territories: ObservableBuffer[UITerritory], 
  initialTroops: Int,
  currentPhase: String 
) extends Stage {
  
  println(s"TroopPlacementDialog inizializzato con ${territories.size} territori e $initialTroops truppe")
  
  val remainingTroops = IntegerProperty(initialTroops)
  
  private val troopAssignments = collection.mutable.Map[String, Int]()
  
  initOwner(owner)
  initModality(Modality.APPLICATION_MODAL)
  initStyle(StageStyle.UTILITY)
  title = "Piazzamento Truppe"
  width = 600
  height = 700
  
  val headerLabel = new Label("Assegna tutte le tue truppe ai territori")
  headerLabel.style = "-fx-font-size: 16px; -fx-font-weight: bold;"
  
  val troopsLabel = new Label()
  troopsLabel.text <== remainingTroops.asString("Truppe rimanenti: %d")
  troopsLabel.style = "-fx-font-size: 14px;"
  
  val territoryControls = territories.map { territory =>
    val nameLabel = new Label(territory.name)
    nameLabel.prefWidth = 200
    nameLabel.style = "-fx-font-weight: bold;"
    
    val currentTroopsLabel = new Label(s"Truppe attuali: ${territory.armies.value}")
    currentTroopsLabel.prefWidth = 100
    
    val troopsSpinner = new Spinner[Int](0, 40, 0)
    troopsSpinner.editable = true
    
    troopAssignments(territory.name) = 0
    
    troopsSpinner.valueProperty().addListener { (_, oldValue, newValue) =>
      val oldVal = oldValue.intValue()
      val newVal = newValue.intValue()
      
      if (newVal > oldVal) {
        val diff = newVal - oldVal
        if (diff > remainingTroops.value) {
          Platform.runLater {
            troopsSpinner.getValueFactory.setValue(oldVal + remainingTroops.value)
          }
        } else {
          remainingTroops.value -= diff
          troopAssignments(territory.name) = newVal
        }
      } else if (newVal < oldVal) {
        val diff = oldVal - newVal
        remainingTroops.value += diff
        troopAssignments(territory.name) = newVal
      }
    }
    
    val row = new HBox(15)
    row.alignment = Pos.CenterLeft
    row.children = Seq(nameLabel, currentTroopsLabel, troopsSpinner)
    
    (row, territory, troopsSpinner)
  }
  
  val territoriesList = new VBox(10)
  territoriesList.padding = Insets(10)
  territoriesList.children = territoryControls.map(_._1)
  
  val scrollPane = new ScrollPane {
    content = territoriesList
    fitToWidth = true
    vbarPolicy = ScrollPane.ScrollBarPolicy.ALWAYS
  }
  
  val confirmButton = new Button("Conferma Piazzamento")
  confirmButton.disable <== remainingTroops =!= 0
  
  confirmButton.onAction = handle {
    troopAssignments.foreach { case (territoryName, troops) =>
      if (troops > 0) {
        val territory = territories.find(_.name == territoryName).get
        val currentTroops = territory.armies.value
        territory.armies.value = currentTroops + troops

        owner.actionHandler.placeTroops(owner.getGameId, territoryName, troops)
      }
    }
    
    if (currentPhase == "SetupPhase") {
      println("[Dialog] Fine piazzamento in fase SetupPhase, invio end_setup")
      owner.actionHandler.endSetup(owner.getGameId)
    } else {
      println("[Dialog] Fine piazzamento in fase MainPhase, chiudo solo il dialogo senza inviare end_turn")
    }
    
    close()
  }
  
  val root = new BorderPane {
    padding = Insets(15)
    top = new VBox(10) {
      children = Seq(headerLabel, troopsLabel)
      alignment = Pos.Center
    }
    center = scrollPane
    bottom = new HBox(10) {
      children = Seq(confirmButton)
      alignment = Pos.CenterRight
      padding = Insets(10, 0, 0, 0)
    }
  }
  
  scene = new Scene(root)
  
  def updateTroops(newTroopsCount: Int): Unit = {
    val assignedTroops = troopAssignments.values.sum
    
    Platform.runLater {
      remainingTroops.value = newTroopsCount - assignedTroops
    }
  }
}