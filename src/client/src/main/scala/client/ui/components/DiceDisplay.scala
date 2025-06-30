package client.ui.components

import scalafx.Includes._
import scalafx.geometry.Pos
import scalafx.scene.control.{Label, Separator}
import scalafx.scene.layout.{HBox, StackPane, VBox}
import scalafx.scene.shape.Rectangle
import scalafx.scene.text.{Font, FontWeight, TextAlignment}
import javafx.scene.paint.Color
import scalafx.application.Platform

/**
 * Componente che visualizza i dadi per l'attacco e la difesa
 */
class DiceDisplay extends VBox(10) {
  
  alignment = Pos.Center
  
  // Migliorare la visibilità del componente
  style = "-fx-background-color: #ffeeee; -fx-border-color: #cc0000; -fx-border-width: 3; -fx-border-radius: 5; -fx-padding: 10;"
  minHeight = 250
  prefHeight = 250
  minWidth = 220
  prefWidth = 220
  maxWidth = 300
  
  // Debug per tracciare il ciclo di vita
  println("DiceDisplay: componente creato")

  // Creiamo un'etichetta per il debug
  private val debugLabel = new Label("Nessun lancio") {
    font = Font.font("Arial", 10)
    textFill = Color.RED
    style = "-fx-background-color: #ffffcc;"
    maxWidth = Double.MaxValue
  }
  
  // Crea dadi con etichette direttamente accessibili
  private class DieWithLabel(color: Color) {
    val label = new Label("?") {
      font = Font.font("Arial", FontWeight.Bold, 24)
      textFill = Color.WHITE
      style = "-fx-font-weight: bold;"
    }
    
    val dieBackground = new Rectangle {
      width = 50
      height = 50
      fill = color
      stroke = Color.BLACK
      strokeWidth = 2
      arcWidth = 10
      arcHeight = 10
    }
    
    val pane = new StackPane {
      alignment = Pos.Center
      children = Seq(dieBackground, label)
      style = "-fx-effect: dropshadow(three-pass-box, rgba(0,0,0,0.6), 5, 0, 0, 0);"
    }
  }
  
  // Collezioni di dadi
  private val attackerDice = Array.fill(3)(new DieWithLabel(Color.RED))
  private val defenderDice = Array.fill(3)(new DieWithLabel(Color.BLUE))
  
  // Contenitori per accesso diretto senza casting
  private val attackerDiceBox = new HBox(10) {
    alignment = Pos.Center
    children = attackerDice.map(_.pane)
  }
  
  private val defenderDiceBox = new HBox(10) {
    alignment = Pos.Center
    children = defenderDice.map(_.pane)
  }
  
  private val attackerDiceSection = createDiceSection("Dadi Attaccante", attackerDiceBox)
  private val defenderDiceSection = createDiceSection("Dadi Difensore", defenderDiceBox)
  
  children = Seq(
    attackerDiceSection,
    new Separator(),
    defenderDiceSection,
    new Separator(),
    debugLabel
  )
  
  // Assicura che il componente sia correttamente visualizzato alla creazione
  Platform.runLater {
    println(s"DiceDisplay: Dimensioni iniziali - width=${width.value}, height=${height.value}, visible=${visible.value}")
  }
  
  private def createDiceSection(title: String, diceBox: HBox): VBox = {
    val titleLabel = new Label(title) {
      font = Font.font("Arial", FontWeight.Bold, 14)
      textAlignment = TextAlignment.Center
      alignment = Pos.Center
      maxWidth = Double.MaxValue
      textFill = if (title.contains("Attaccante")) Color.DARKRED else Color.DARKBLUE
    }
    
    new VBox(5) {
      alignment = Pos.Center
      style = "-fx-padding: 5;"
      children = Seq(titleLabel, diceBox)
    }
  }
  
  /**
   * Aggiorna i valori dei dadi visualizzati
   */
  def updateValues(attackerValues: List[Int], defenderValues: List[Int]): Unit = {
    println(s"DiceDisplay: Aggiornamento dadi - Attaccante: ${attackerValues.mkString(",")}, Difensore: ${defenderValues.mkString(",")}")
    
    // Debug delle dimensioni del componente
    println(s"DiceDisplay: Dimensioni - width=${width.value}, height=${height.value}, visible=${visible.value}")
    
    // Verifica se il componente è presente nella gerarchia visuale
    def isNodeInScene(node: javafx.scene.Node): Boolean = {
      node.getScene != null && node.getScene.getWindow != null && node.getScene.getWindow.isShowing
    }
    println(s"DiceDisplay: Presente nella scena: ${isNodeInScene(this)}")

    // Aggiorna il testo di debug
    debugLabel.text = s"Attacco: ${attackerValues.mkString(",")} vs Difesa: ${defenderValues.mkString(",")}"
    
    Platform.runLater {
      try {
        println("DiceDisplay: Esecuzione runLater per aggiornare i dadi...")
        
        // Resetta tutti i dadi a "?"
        for (i <- 0 until 3) {
          // Resetta dadi attaccante
          attackerDice(i).label.text = "?"
          attackerDice(i).label.style = "-fx-font-weight: bold;"
          attackerDice(i).label.font = Font.font("Arial", FontWeight.Bold, 24)
          attackerDice(i).pane.visible = true
          attackerDice(i).pane.opacity = 1.0
          attackerDice(i).pane.style = "-fx-effect: dropshadow(three-pass-box, rgba(0,0,0,0.6), 5, 0, 0, 0);"
          
          // Resetta dadi difensore
          defenderDice(i).label.text = "?"
          defenderDice(i).label.style = "-fx-font-weight: bold;"
          defenderDice(i).label.font = Font.font("Arial", FontWeight.Bold, 24)
          defenderDice(i).pane.visible = true
          defenderDice(i).pane.opacity = 1.0
          defenderDice(i).pane.style = "-fx-effect: dropshadow(three-pass-box, rgba(0,0,0,0.6), 5, 0, 0, 0);"
        }
        
        // Imposta i valori effettivi per i dadi usati
        attackerValues.zipWithIndex.foreach { case (value, index) =>
          if (index < 3) {
            attackerDice(index).label.text = value.toString
            // Aumenta la visibilità e la dimensione del font per evidenziare
            attackerDice(index).label.font = Font.font("Arial", FontWeight.ExtraBold, 26)
            attackerDice(index).label.style = "-fx-font-weight: extra-bold; -fx-effect: dropshadow(gaussian, rgba(255,255,255,0.5), 2, 0, 0, 0);"
            
            // Enfatizza il dado
            attackerDice(index).pane.style = "-fx-effect: dropshadow(three-pass-box, rgba(255,0,0,0.8), 15, 0, 0, 0);"
            println(s"DiceDisplay: Dado attaccante ${index + 1} impostato a $value")
          }
        }
        
        defenderValues.zipWithIndex.foreach { case (value, index) =>
          if (index < 3) {
            defenderDice(index).label.text = value.toString
            // Aumenta la visibilità e la dimensione del font per evidenziare
            defenderDice(index).label.font = Font.font("Arial", FontWeight.ExtraBold, 26)
            defenderDice(index).label.style = "-fx-font-weight: extra-bold; -fx-effect: dropshadow(gaussian, rgba(255,255,255,0.5), 2, 0, 0, 0);"
            
            // Enfatizza il dado
            defenderDice(index).pane.style = "-fx-effect: dropshadow(three-pass-box, rgba(0,0,255,0.8), 15, 0, 0, 0);"
            println(s"DiceDisplay: Dado difensore ${index + 1} impostato a $value")
          }
        }
        
        // Disabilita i dadi non utilizzati
        (attackerValues.size until 3).foreach { index =>
          attackerDice(index).pane.opacity = 0.3
        }
        
        (defenderValues.size until 3).foreach { index =>
          defenderDice(index).pane.opacity = 0.3
        }
        
        println("DiceDisplay: Aggiornamento dei dadi completato")
        
      } catch {
        case ex: Exception =>
          println(s"DiceDisplay: ERRORE durante l'aggiornamento dei dadi: ${ex.getMessage}")
          ex.printStackTrace()
      }
    }
  }
}