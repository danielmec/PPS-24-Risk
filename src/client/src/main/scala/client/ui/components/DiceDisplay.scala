package client.ui.components

import scalafx.Includes._
import scalafx.geometry.Pos
import scalafx.scene.control.{Label, Separator}
import scalafx.scene.layout.{HBox, StackPane, VBox}
import scalafx.scene.shape.Rectangle
import scalafx.scene.text.{Font, FontWeight, TextAlignment}
import javafx.scene.paint.Color

/**
 * Componente che visualizza i dadi per l'attacco e la difesa
 */
class DiceDisplay extends VBox(10) {
  
  alignment = Pos.Center
  children = Seq(
    createDiceSection("Dadi Attaccante", Color.RED),
    new Separator(),
    createDiceSection("Dadi Difensore", Color.BLUE)
  )
  style = "-fx-background-color: #f0f0f0; -fx-border-color: #cccccc; -fx-border-radius: 5; -fx-padding: 10;"
  
  private def createDiceSection(title: String, color: Color): VBox = {
    val titleLabel = new Label(title) {
      font = Font.font("Arial", FontWeight.Bold, 14)
      textAlignment = TextAlignment.Center
      alignment = Pos.Center
      maxWidth = Double.MaxValue
    }
    
    val diceRow = new HBox(5) {
      alignment = Pos.Center
      children = (1 to 3).map(_ => createDie(color))
    }
    
    new VBox(5) {
      alignment = Pos.Center
      children = Seq(titleLabel, diceRow)
    }
  }
  
  private def createDie(color: Color): StackPane = {
    val dieValue = new Label("?") {
      font = Font.font("Arial", FontWeight.Bold, 16)
      textFill = Color.WHITE
    }
    
    val dieBackground = new Rectangle {
      width = 40
      height = 40
      fill = color
      stroke = Color.BLACK
      strokeWidth = 1
      arcWidth = 5
      arcHeight = 5
    }
    
    new StackPane {
      children = Seq(dieBackground, dieValue)
    }
  }
  
  /**
   * Aggiorna i valori dei dadi visualizzati
   */
  def updateValues(attackerValues: List[Int], defenderValues: List[Int]): Unit = {
    //valori dadi 
  }
}