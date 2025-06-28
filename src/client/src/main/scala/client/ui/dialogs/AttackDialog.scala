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
import scala.concurrent.ExecutionContext.Implicits.global

/**
 * Dialog per gestire un attacco tra territori
 */
class AttackDialog(
  owner: GameWindow,
  myTerritories: ObservableBuffer[UITerritory],
  enemyTerritories: ObservableBuffer[UITerritory]
) extends Stage {

  // Configurazione finestra
  title = "Attacco"
  initOwner(owner)
  initModality(Modality.APPLICATION_MODAL)
  width = 400
  height = 280
  
  // Converter per visualizzare i territori in modo leggibile
  private val territoryConverter = new StringConverter[UITerritory] {
    override def toString(territory: UITerritory): String = 
      if (territory == null) "" else s"${territory.name} (${territory.armies.value} truppe)"
    
    override def fromString(string: String): UITerritory = null
  }

  // Componenti UI
  private val attackerCombo = new ComboBox[UITerritory] {
    items = ObservableBuffer.from(myTerritories.filter(_.armies.value > 1))
    promptText = "Seleziona un territorio"
    converter = territoryConverter
    
    onAction = _ => updateDefenderOptions()
  }
  
  private val defenderCombo = new ComboBox[UITerritory] {
    promptText = "Seleziona un territorio"
    disable = true
    converter = territoryConverter
    
    onAction = _ => updateTroopsControl()
  }
  
  private val troopsSpinner = new Spinner[Int](1, 3, 1) {
    disable = true
    editable = true
  }
  
  private val attackButton = new Button("Attacca") {
    style = "-fx-base: #ffaaaa;"
    disable = true
    onAction = _ => executeAttack()
  }
  
  // Layout
  scene = new Scene {
    root = new VBox(15) {
      padding = Insets(20)
      alignment = Pos.Center
      children = Seq(
        new Label("Seleziona i territori per l'attacco") { style = "-fx-font-size: 16px; -fx-font-weight: bold;" },
        new GridPane {
          hgap = 10
          vgap = 15
          padding = Insets(10)
          
          add(new Label("Territorio attaccante:") { style = "-fx-font-weight: bold;" }, 0, 0)
          add(attackerCombo, 1, 0)
          
          add(new Label("Territorio da attaccare:") { style = "-fx-font-weight: bold;" }, 0, 1)
          add(defenderCombo, 1, 1)
          
          add(new Label("Truppe da usare:") { style = "-fx-font-weight: bold;" }, 0, 2)
          add(troopsSpinner, 1, 2)
        },
        new HBox(10) {
          alignment = Pos.CenterRight
          children = Seq(
            attackButton,
            new Button("Annulla") {
              onAction = _ => close()
            }
          )
        }
      )
    }
  }

  /**
   * Aggiorna le opzioni di difesa in base al territorio attaccante selezionato
   */
  private def updateDefenderOptions(): Unit = {
    Option(attackerCombo.value.value).foreach { attacker =>
      // Trova territori adiacenti nemici
      val adjacentEnemies = enemyTerritories.filter(attacker.isNeighbor)
      
      if (adjacentEnemies.isEmpty) {
        defenderCombo.items = ObservableBuffer.empty
        defenderCombo.disable = true
        defenderCombo.promptText = "Nessun territorio adiacente"
      } else {
        defenderCombo.items = ObservableBuffer.from(adjacentEnemies)
        defenderCombo.disable = false
        defenderCombo.promptText = "Seleziona un territorio"
      }
      
      defenderCombo.selectionModel().clearSelection()
      troopsSpinner.disable = true
      attackButton.disable = true
    }
  }

  /**
   * Aggiorna il controllo delle truppe in base ai territori selezionati
   */
  private def updateTroopsControl(): Unit = {
    for {
      attacker <- Option(attackerCombo.value.value)
      defender <- Option(defenderCombo.value.value)
    } yield {
      // Configura spinner per numero di truppe (max 3, minimo 1, non può lasciare il territorio vuoto)
      val maxTroops = math.min(3, attacker.armies.value - 1)
      
      // Soluzione con cast che risolve gli errori di tipo
      troopsSpinner.valueFactory = {
        val factory = new SpinnerValueFactory.IntegerSpinnerValueFactory(1, maxTroops, 1)
        factory.asInstanceOf[SpinnerValueFactory[Int]]
      }
      
      troopsSpinner.disable = false
      attackButton.disable = false
    }
  }

  /**
   * Esegue l'attacco con i valori selezionati
   */
  private def executeAttack(): Unit = {
    for {
      attacker <- Option(attackerCombo.value.value)
      defender <- Option(defenderCombo.value.value)
      troops = troopsSpinner.value.value
    } yield {
      // Esegui attacco
      owner.actionHandler.attack(
        owner.getGameId, 
        attacker.name, 
        defender.name, 
        troops,
        defender.owner.value
      ).onComplete {
        case scala.util.Success(true) => 
          println("[UI] Attacco inviato con successo")
        case _ => 
          Platform.runLater {
            new Alert(Alert.AlertType.Error) {
              initOwner(AttackDialog.this)
              title = "Errore"
              headerText = "Attacco fallito"
              contentText = "Non è stato possibile effettuare l'attacco."
            }.showAndWait()
          }
      }
      
      // Chiudi il dialog
      close()
    }
  }
}