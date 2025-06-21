package client.ui.dialogs

import scalafx.Includes._
import scalafx.application.Platform
import scalafx.geometry.{Insets, Pos}
import scalafx.scene.Scene
import scalafx.scene.control._
import scalafx.scene.layout._
import scalafx.stage.{Modality, Stage, StageStyle}
import scalafx.collections.ObservableBuffer
import scalafx.beans.property.{IntegerProperty, ObjectProperty}
import scalafx.beans.binding.Bindings
import client.AdapterMap.UITerritory
import client.ui.GameWindow

/**
 * Finestra di dialogo per il piazzamento delle truppe sui territori
 *
 * @param owner La finestra proprietaria
 * @param territories Lista dei territori su cui piazzare le truppe
 * @param initialTroops Numero totale di truppe disponibili
 * @param onPlaceTroops Callback da invocare quando si piazzano le truppe
 */
class TroopPlacementDialog(
  owner: GameWindow,
  territories: ObservableBuffer[UITerritory], 
  initialTroops: Int,
  onPlaceTroops: (UITerritory, Int) => Unit //callback per piazzare le truppe
) extends Stage {
  
  println(s"TroopPlacementDialog inizializzato con ${territories.size} territori e $initialTroops truppe")
  territories.foreach(t => println(s"Territorio disponibile: ${t.name}, Owner: ${t.owner.value}, Truppe: ${t.armies.value}"))
  
  // proprietà per tenere traccia delle truppe rimanenti
  val remainingTroops = IntegerProperty(initialTroops)
  
  // Configurazione della finestra
  initOwner(owner)
  initModality(Modality.ApplicationModal)
  initStyle(StageStyle.Utility)
  title = "Piazzamento Truppe"
  width = 650
  height = 700
  
  
  val headerLabel = new Label("Piazza le tue truppe sui territori")
  headerLabel.style = "-fx-font-size: 16px; -fx-font-weight: bold;"
  
  val troopsLabel = new Label()
  troopsLabel.text <== remainingTroops.asString("Truppe rimanenti: %d")
  troopsLabel.style = "-fx-font-size: 14px;"
  
  val territoriesList = new VBox(5)
  territoriesList.padding = Insets(10)
  
  val territoryControls = territories.map { territory =>
    //spinner è un componente per selezionare il numero di truppe da piazzare
    val troopsSpinner = new Spinner[Int](0, 40, 0)
    troopsSpinner.editable = true
    
    val nameLabel = new Label(territory.name)
    nameLabel.prefWidth = 200
    nameLabel.style = "-fx-font-weight: bold;"
    
    val currentTroopsLabel = new Label(s"Truppe attuali: ${territory.armies.value}")
    currentTroopsLabel.prefWidth = 100
    
    val placeButton = new Button("Piazza")
    
    // imposta il valore massimo dello spinner in base alle truppe rimanenti
    placeButton.disable <== Bindings.createBooleanBinding(
      () => troopsSpinner.value.value == 0,
      troopsSpinner.value
    )
    
    placeButton.onAction = handle {
      val troopsToPlace = troopsSpinner.value.value
      
      if (troopsToPlace > 0 && troopsToPlace <= remainingTroops.value) {
        
        val currentTroops = territory.armies.value
        territory.armies.value = currentTroops + troopsToPlace
        
        currentTroopsLabel.text = s"Truppe attuali: ${territory.armies.value}"
        // invia al callback per piazzare le truppe
        onPlaceTroops(territory, troopsToPlace)
        
        remainingTroops.value -= troopsToPlace
      
        Platform.runLater {
          troopsSpinner.getValueFactory.setValue(0)
        }
      }
    }
    
    // aggiorna il valore massimo dello spinner in base alle truppe rimanenti
    troopsSpinner.valueProperty().addListener { (_, _, newValue) =>
      if (newValue.intValue() > remainingTroops.value) {
        Platform.runLater {
          troopsSpinner.getValueFactory.setValue(remainingTroops.value)
        }
      }
    }
    //per ogni territorio crea una riga con i controlli
    val row = new HBox(10)
    row.alignment = Pos.CenterLeft
    row.children = Seq(nameLabel, currentTroopsLabel, troopsSpinner, placeButton)
    
    row
  }
  

  territoriesList.children = territoryControls
  
  val scrollPane = new ScrollPane {
    content = territoriesList
    fitToWidth = true
    vbarPolicy = ScrollPane.ScrollBarPolicy.Always
  }
  
  val finishButton = new Button("Termina Piazzamento")
  finishButton.onAction = handle {
    
    if (remainingTroops.value > 0) {
      val alert = new Alert(Alert.AlertType.Confirmation) {
        initOwner(TroopPlacementDialog.this)
        title = "Conferma"
        headerText = "Hai ancora truppe da piazzare"
        contentText = s"Hai ancora ${remainingTroops.value} truppe da piazzare. Sei sicuro di voler terminare il piazzamento?"
      }
      
      val result = alert.showAndWait()
      
      if (result.contains(ButtonType.OK)) {
        owner.actionHandler.endPhase(owner.getGameId)
        close()
      }
    } else {
      owner.actionHandler.endPhase(owner.getGameId)
      close()
    }
  }
  
  // Layout principale
  val root = new BorderPane {
    padding = Insets(15)
    top = new VBox(10) {
      children = Seq(headerLabel, troopsLabel)
      alignment = Pos.Center
    }
    center = scrollPane
    bottom = new HBox(10) {
      children = Seq(finishButton)
      alignment = Pos.CenterRight
      padding = Insets(10, 0, 0, 0)
    }
  }
  
  scene = new Scene(root)
  
}