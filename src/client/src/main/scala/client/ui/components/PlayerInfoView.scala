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

  val playerId = {
    val idPattern = ".*\\((.+?)\\)$".r
    playerName match {
      case idPattern(id) => id
      case _ => playerName
    }
  }
  
  
  val colorMap = Map(
    "RED" -> Color.RED,
    "BLUE" -> Color.BLUE,
    "GREEN" -> Color.GREEN,
    "YELLOW" -> Color.YELLOW,
    "ORANGE" -> Color.ORANGE,
    "PURPLE" -> Color.PURPLE,
    "BROWN" -> Color.BROWN
  )
  
  val serverColor = playerColors.get(playerId)
  println(s"[PlayerInfoView] Colore dal server per $playerId: $serverColor")
  
  //  convertire il nome del colore
  val convertedColor = serverColor.flatMap { colorName => 
    val color = colorMap.get(colorName.toUpperCase)
    println(s"[PlayerInfoView] Tentativo conversione '$colorName' -> $color")
    color
  }

  // Usa il colore dal server se disponibile, altrimenti usa la logica di fallback
  val playerColor = convertedColor.getOrElse {
    println(s"[PlayerInfoView] Usando colore di fallback per $playerId")
    val colors = List(Color.BROWN)
    val playerIndex = currentPlayers.indexOf(playerName) 
    val fallbackColor = if (playerIndex >= 0) colors(playerIndex % colors.size) else Color.GRAY
    println(s"[PlayerInfoView] Colore di fallback scelto: $fallbackColor")
    fallbackColor
  }
  
  println(s"[PlayerInfoView] Colore finale per $playerName: $playerColor")
  
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

