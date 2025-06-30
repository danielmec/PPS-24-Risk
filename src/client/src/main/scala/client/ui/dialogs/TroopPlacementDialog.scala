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
 * Finestra di dialogo semplificata per il piazzamento delle truppe
 */
class TroopPlacementDialog(
  owner: GameWindow,
  territories: ObservableBuffer[UITerritory], 
  initialTroops: Int,
  currentPhase: String // Nuovo parametro
) extends Stage {
  
  println(s"TroopPlacementDialog inizializzato con ${territories.size} territori e $initialTroops truppe")
  
  val remainingTroops = IntegerProperty(initialTroops)
  
  //mappa usata per tenere traccia delle assegnazioni di truppe per territorio
  private val troopAssignments = collection.mutable.Map[String, Int]()
  
  // Configurazione della finestra
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
        // L'utente sta aumentando le truppe
        val diff = newVal - oldVal
        if (diff > remainingTroops.value) {
          //non ci sono abbastanza truppe disponibili
          Platform.runLater {
            troopsSpinner.getValueFactory.setValue(oldVal + remainingTroops.value)
          }
        } else {
          //aggiorna le truppe rimanenti
          remainingTroops.value -= diff
          troopAssignments(territory.name) = newVal
        }
      } else if (newVal < oldVal) {
        // L'utente sta diminuendo le truppe
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
  
  // Pulsante per confermare il piazzamento
  val confirmButton = new Button("Conferma Piazzamento")
  confirmButton.disable <== remainingTroops =!= 0 // Disabilita se ci sono ancora truppe da piazzare
  
  confirmButton.onAction = handle {
    // Invia tutti i piazzamenti al server
    troopAssignments.foreach { case (territoryName, troops) =>
      if (troops > 0) {
        val territory = territories.find(_.name == territoryName).get
        val currentTroops = territory.armies.value
        territory.armies.value = currentTroops + troops
        
        // Invia al server
        owner.actionHandler.placeTroops(owner.getGameId, territoryName, troops)
      }
    }
    
    // CORREZIONE: Non terminiamo più automaticamente il turno dopo il piazzamento truppe
    // Solo nella fase SetupPhase inviamo un end_setup, nella MainPhase lasciamo che il giocatore
    // continui con altre azioni (attacchi, rinforzi o fine turno manuale)
    if (currentPhase == "SetupPhase") {
      println("[Dialog] Fine piazzamento in fase SetupPhase, invio end_setup")
      owner.actionHandler.endSetup(owner.getGameId)
    } else {
      println("[Dialog] Fine piazzamento in fase MainPhase, chiudo solo il dialogo senza inviare end_turn")
      // Non facciamo nulla, il giocatore può continuare con altre azioni
    }
    
    close()
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
      children = Seq(confirmButton)
      alignment = Pos.CenterRight
      padding = Insets(10, 0, 0, 0)
    }
  }
  
  scene = new Scene(root)
  
  // Metodo per aggiornare il numero di truppe disponibili
  def updateTroops(newTroopsCount: Int): Unit = {
    // calcola quante truppe sono già state assegnate ma non ancora inviate
    val assignedTroops = troopAssignments.values.sum
    
    Platform.runLater {
      remainingTroops.value = newTroopsCount - assignedTroops
    }
  }
}