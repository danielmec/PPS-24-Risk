package client.ui.components

import scalafx.Includes._
import scalafx.geometry.Insets
import scalafx.geometry.Pos
import scalafx.scene.control.Label
import scalafx.scene.layout.Priority
import scalafx.scene.layout.HBox
import scalafx.scene.shape.Rectangle
import scalafx.scene.text.FontWeight
import scalafx.scene.text.Font
import javafx.scene.paint.Color
import client.ClientNetworkManager


class PlayerInfoView(
  playerName: String, 
  currentPlayers: List[String], 
  networkManager: ClientNetworkManager,
  playerColors: Map[String, String] = Map.empty
) extends HBox(10) {

  // Estrai l'ID del giocatore dalla stringa "username (id)"
  val playerId = {
    val idPattern = ".*\\((.+?)\\)$".r
    playerName match {
      case idPattern(id) => id
      case _ => playerName
    }
  }
  
  // Mappa semplice di nomi colori a oggetti Color
  val colorMap = Map(
    "RED" -> Color.RED,
    "BLUE" -> Color.BLUE,
    "GREEN" -> Color.GREEN,
    "YELLOW" -> Color.YELLOW,
    "ORANGE" -> Color.ORANGE,
    "PURPLE" -> Color.PURPLE,
    "BROWN" -> Color.BROWN
  )
  
  // Usa il colore dal server se disponibile, altrimenti usa la logica di fallback
  val playerColor = playerColors.get(playerId)
    .flatMap(colorName => colorMap.get(colorName))
    .getOrElse {
      val colors = List(Color.RED, Color.BLUE, Color.GREEN, Color.ORANGE, Color.PURPLE, Color.BROWN)
      val playerIndex = currentPlayers.indexOf(playerName) 
      if (playerIndex >= 0) colors(playerIndex % colors.size) else Color.GRAY
    }
  
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
    

  }
  
  padding = Insets(5)
  alignment = Pos.CenterLeft
  style = "-fx-border-color: #cccccc; -fx-border-radius: 5; -fx-background-radius: 5; -fx-background-color: white;"
  children = Seq(colorIndicator, nameLabel)
}

