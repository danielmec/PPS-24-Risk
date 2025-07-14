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

/**
  * A UI component that displays information about a player,
  * including their name and a colored indicator representing the player's color.
  * 
  * The player color is determined from the provided playerColors map, falling back to a default color logic
  * if the server color is not available or cannot be converted.
  *
  * @param playerName The full name of the player (may contain an ID in parentheses).
  * @param currentPlayers The list of player names currently in the game.
  * @param networkManager The client network manager instance (used for network-related interactions).
  * @param playerColors Optional map of player IDs to their assigned color names as strings.
  */
class PlayerInfoView(
  playerName: String, 
  currentPlayers: List[String], 
  networkManager: ClientNetworkManager,
  playerColors: Map[String, String] = Map.empty
) extends HBox(10) {

  /**
    * Extracts the player ID from the player name if it is enclosed in parentheses.
    * Otherwise, returns the full playerName.
    */
  val playerId = {
    val idPattern = ".*\\((.+?)\\)$".r
    playerName match {
      case idPattern(id) => id
      case _ => playerName
    }
  }
  
  /**
    * Map from color names (uppercase) to JavaFX Color objects.
    */
  val colorMap = Map(
    "RED" -> Color.RED,
    "BLUE" -> Color.BLUE,
    "GREEN" -> Color.GREEN,
    "YELLOW" -> Color.YELLOW,
    "ORANGE" -> Color.ORANGE,
    "PURPLE" -> Color.PURPLE,
    "BROWN" -> Color.BROWN
  )
  
  /**
    * Attempts to get the player's color name from the server-provided map.
    */
  val serverColor = playerColors.get(playerId)
  
  /**
    * Converts the server-provided color name to a JavaFX Color, if possible.
    */
  val convertedColor = serverColor.flatMap { colorName => 
    val color = colorMap.get(colorName.toUpperCase)
    color
  }

  /**
    * The color used to represent the player.
    * Uses the converted server color if available, otherwise falls back to a default color logic.
    */
  val playerColor = convertedColor.getOrElse {
    val colors = List(Color.BROWN)
    val playerIndex = currentPlayers.indexOf(playerName) 
    if (playerIndex >= 0) colors(playerIndex % colors.size) else Color.GRAY
  }
  
  /**
    * A small rectangle acting as a color indicator for the player.
    */
  val colorIndicator = new Rectangle {
    width = 15
    height = 15
    fill = playerColor
    stroke = Color.BLACK
    strokeWidth = 1
  }
  
  /**
    * Label displaying the player's name.
    * The label expands to take available horizontal space.
    */
  val nameLabel = new Label(playerName) {
    maxWidth = Double.MaxValue
    HBox.setHgrow(this, Priority.Always)
  }
  
  padding = Insets(5)
  alignment = Pos.CenterLeft
  style = "-fx-border-color: #cccccc; -fx-border-radius: 5; -fx-background-radius: 5; -fx-background-color: white;"
  children = Seq(colorIndicator, nameLabel)
}
