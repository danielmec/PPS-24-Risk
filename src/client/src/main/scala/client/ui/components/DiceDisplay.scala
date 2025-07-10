package client.ui.components

import scalafx.Includes._
import scalafx.geometry.Pos
import scalafx.scene.control.Separator
import scalafx.scene.control.Label
import scalafx.scene.layout.StackPane
import scalafx.scene.layout.HBox
import scalafx.scene.layout.VBox
import scalafx.scene.shape.Rectangle
import scalafx.scene.text.TextAlignment
import scalafx.scene.text.Font
import scalafx.scene.text.FontWeight
import javafx.scene.paint.Color
import scalafx.application.Platform


/**
  * This class represents a display for the dice used in the game.
  * It shows the dice for both the attacker and defender, along with their values.
  * The dice are displayed in a visually appealing way, with colors and styles.
  */ 
class DiceDisplay extends VBox(10) {
  
  alignment = Pos.Center
  style = "-fx-background-color: #ffeeee; -fx-border-color: #cc0000; -fx-border-width: 3; -fx-border-radius: 5; -fx-padding: 10;"
  minHeight = 250
  prefHeight = 250
  minWidth = 220
  prefWidth = 220
  maxWidth = 300
  
  println("DiceDisplay: componente creato")

  private val debugLabel = new Label("Nessun lancio") {
    font = Font.font("Arial", 10)
    textFill = Color.RED
    style = "-fx-background-color: #ffffcc;"
    maxWidth = Double.MaxValue
  }

  /**
    * This class represents a die with a label that displays its value.
    * @param color
    */
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
  
  private val attackerDice = Array.fill(3)(new DieWithLabel(Color.RED))
  private val defenderDice = Array.fill(3)(new DieWithLabel(Color.BLUE))
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
  
  Platform.runLater {
    println(s"DiceDisplay: Dimensioni iniziali - width=${width.value}, height=${height.value}, visible=${visible.value}")
  }
  
  /**
    * This method creates a VBox containing a title label and a HBox for the dice.
    * @param title
    * @param diceBox is an HBox (container for the dice) that will be displayed below the title.
    * @return a VBox with the title and dice box
    */
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

  //coda di animazione per gestire le animazioni dei dadi in sequenza per i bot che attaccano a sequenza
  private val animationQueue = new scala.collection.mutable.Queue[(List[Int], List[Int])]()
  private var animationInProgress = false
  

  /**
    * Updates the values of the attacker and defender dice.
    * @param attackerValues
    * @param defenderValues
    */
  def updateValues(attackerValues: List[Int], defenderValues: List[Int]): Unit = {
    println(s"DiceDisplay: Accodamento dadi - Attaccante: ${attackerValues.mkString(",")}, Difensore: ${defenderValues.mkString(",")}")
    
    //inizia l'animazione se non è già in corso
    animationQueue.enqueue((attackerValues, defenderValues))
    if (!animationInProgress) {
      processNextAnimation()
    }
  }
  

  /**
    * Processes the next animation in the queue.
    * It updates the dice display with the values from the queue.
    * If the queue is empty, it sets animationInProgress to false.
    */
  private def processNextAnimation(): Unit = {
    if (animationQueue.isEmpty) {
      animationInProgress = false
      return
    }
    
    animationInProgress = true
    val (attackerValues, defenderValues) = animationQueue.dequeue()
    
    Platform.runLater {
      try {
        println(s"DiceDisplay: Animazione dadi - Attaccante: ${attackerValues.mkString(",")}, Difensore: ${defenderValues.mkString(",")}")

        for (i <- 0 until 3) {
          attackerDice(i).label.text = "?"
          attackerDice(i).label.style = "-fx-font-weight: bold;"
          attackerDice(i).label.font = Font.font("Arial", FontWeight.Bold, 24)
          attackerDice(i).pane.visible = true
          attackerDice(i).pane.opacity = 1.0
          attackerDice(i).pane.style = "-fx-effect: dropshadow(three-pass-box, rgba(0,0,0,0.6), 5, 0, 0, 0);"
          
          defenderDice(i).label.text = "?"
          defenderDice(i).label.style = "-fx-font-weight: bold;"
          defenderDice(i).label.font = Font.font("Arial", FontWeight.Bold, 24)
          defenderDice(i).pane.visible = true
          defenderDice(i).pane.opacity = 1.0
          defenderDice(i).pane.style = "-fx-effect: dropshadow(three-pass-box, rgba(0,0,0,0.6), 5, 0, 0, 0);"
        }
        
        attackerValues.zipWithIndex.foreach { case (value, index) =>
          if (index < 3) {
            attackerDice(index).label.text = value.toString
            attackerDice(index).label.font = Font.font("Arial", FontWeight.ExtraBold, 26)
            attackerDice(index).label.style = "-fx-font-weight: extra-bold; -fx-effect: dropshadow(gaussian, rgba(255,255,255,0.5), 2, 0, 0, 0);"
            attackerDice(index).pane.style = "-fx-effect: dropshadow(three-pass-box, rgba(255,0,0,0.8), 15, 0, 0, 0);"
            println(s"DiceDisplay: Dado attaccante ${index + 1} impostato a $value")
          }
        }
        
        defenderValues.zipWithIndex.foreach { case (value, index) =>
          if (index < 3) {
            defenderDice(index).label.text = value.toString
            defenderDice(index).label.font = Font.font("Arial", FontWeight.ExtraBold, 26)
            defenderDice(index).label.style = "-fx-font-weight: extra-bold; -fx-effect: dropshadow(gaussian, rgba(255,255,255,0.5), 2, 0, 0, 0);"
            defenderDice(index).pane.style = "-fx-effect: dropshadow(three-pass-box, rgba(0,0,255,0.8), 15, 0, 0, 0);"
            println(s"DiceDisplay: Dado difensore ${index + 1} impostato a $value")
          }
        }
        
        val originalStyle = style.value
        style = originalStyle + "-fx-background-color: #ffcccc;"
        
        //prrogramma il prossimo aggiornamento dopo 3.5 secondi
        new java.util.Timer().schedule(new java.util.TimerTask {
          override def run(): Unit = {
            Platform.runLater {
              style = originalStyle
              // breve ritardo
              new java.util.Timer().schedule(new java.util.TimerTask {
                override def run(): Unit = {
                  Platform.runLater {
                    processNextAnimation()
                  }
                }
              }, 500) // 0.5 secondi di pausa tra animazioni
            }
          }
        }, 3000) // 3 secondi di visualizzazione per ogni set di dadi
        
      } catch {
        case ex: Exception =>
          println(s"DiceDisplay: ERRORE durante l'animazione dei dadi: ${ex.getMessage}")
          ex.printStackTrace()
      
          processNextAnimation()
      }
    }
  }


}