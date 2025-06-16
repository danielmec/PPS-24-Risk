package client.ui.components

import scalafx.Includes._
import scalafx.geometry.{Insets, Pos}
import scalafx.scene.control.Label
import scalafx.scene.layout.{HBox, Priority}
import scalafx.scene.shape.Rectangle
import scalafx.scene.text.{Font, FontWeight}
import javafx.scene.paint.Color
import client.ClientNetworkManager

/**
 * Componente che visualizza le informazioni di un giocatore
 */
class PlayerInfoView(playerName: String, currentPlayers: List[String], networkManager: ClientNetworkManager) extends HBox(10) {
  // Assegna colori diversi ai giocatori
  val colors = List(
    Color.RED, Color.BLUE, Color.GREEN, Color.ORANGE, Color.PURPLE, Color.BROWN
  )
  val playerIndex = currentPlayers.indexOf(playerName) 
  val playerColor = if (playerIndex >= 0) colors(playerIndex % colors.size) else Color.GRAY
  
  //Crea un indicatore di colore per il giocatore
  val colorIndicator = new Rectangle {
    width = 15
    height = 15
    fill = playerColor
    stroke = Color.BLACK
    strokeWidth = 1
  }
  
  val nameLabel = new Label(playerName) {
    maxWidth = Double.MaxValue
    HBox.setHgrow(this, Priority.Always)
    
    //evidenzia il giocatore corrente
    if (networkManager.getPlayerId.exists(id => playerName.endsWith(id))) {
      font = Font.font("Arial", FontWeight.Bold, 13)
      style = "-fx-underline: true;"
    }
  }
  
  //proprietà della vista giocatore
  padding = Insets(5)
  alignment = Pos.CenterLeft
  style = "-fx-border-color: #cccccc; -fx-border-radius: 5; -fx-background-radius: 5; -fx-background-color: white;"
  children = Seq(colorIndicator, nameLabel)
}

