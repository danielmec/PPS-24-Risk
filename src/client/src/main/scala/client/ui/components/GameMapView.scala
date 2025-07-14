package client.ui.components

import scalafx.Includes._
import scalafx.scene.control.Label
import scalafx.scene.control.Tooltip
import scalafx.scene.control.ScrollPane
import scalafx.scene.image.Image
import scalafx.scene.image.ImageView
import scalafx.scene.layout.StackPane
import javafx.scene.paint.Color
import scalafx.scene.text.Font
import scalafx.scene.text.FontWeight

/**
  * Represents the game map view inside a ScrollPane.
  * Loads the map image from a file and displays it using an ImageView.
  * Handles resizing while maintaining the correct aspect ratio.
  * Executes the given callback when the map is clicked.
  *
  * @param onMapClicked Code block to execute when the map is clicked.
  */
class GameMapView(onMapClicked: => Unit) extends ScrollPane {
  
  /**
    * The map image loaded from a file.
    * If loading fails, this will be null and an error message will be printed.
    */
  val mapImage = try {
    new Image("file:src/client/src/main/scala/client/ui/resources/RiskMap.drawio.png")
  } catch {
    case e: Exception => 
      println(s"Error loading map image: ${e.getMessage}")
      null
  }
  
  /**
    * ImageView used to display the map image.
    * Sets properties to preserve aspect ratio and smooth scaling.
    * Registers mouse click event to trigger the provided callback.
    * Changes cursor to hand on hover.
    */
  val mapView = new ImageView {
    if (mapImage != null) {
      image = mapImage
      preserveRatio = true
      smooth = true
      
      onMouseClicked = event => onMapClicked
      
      cursor = javafx.scene.Cursor.HAND
    }
  }
  
  /**
    * Container StackPane that holds the map view.
    * Shows a label with an error message if the map image is not available.
    */
  val mapContainer = new StackPane {
    children = Seq(
      if (mapImage != null) mapView else new Label("Map not available") {
        font = Font.font("Arial", FontWeight.Bold, 14)
        textFill = Color.RED
      }
    )
    style = "-fx-background-color: #f0f0f0;"
  }
  
  /** Sets the ScrollPane content to the map container */
  content = mapContainer
  
  /** Tooltip inviting users to click the map to view all territories */
  val mapTooltip = new Tooltip("Click on the map to view all territories")
  if (mapImage != null) {
    Tooltip.install(mapView, mapTooltip)
  }

  /** Disables horizontal and vertical scrollbars and fits content to ScrollPane size */
  hbarPolicy = ScrollPane.ScrollBarPolicy.NEVER
  vbarPolicy = ScrollPane.ScrollBarPolicy.NEVER
  fitToWidth = true
  fitToHeight = true
  pannable = false
  
  /**
    * Binds the map view size to the given scene dimensions.
    * Adjusts the map size while preserving the aspect ratio.
    *
    * @param sceneWidth Current width of the scene.
    * @param sceneHeight Current height of the scene.
    */
  def bindToSceneDimensions(sceneWidth: => Double, sceneHeight: => Double): Unit = {
    if (mapImage != null) {
      
      mapView.fitWidth = 100
      mapView.fitHeight = 100
      
      width.onChange { (_, _, newWidth) =>
        height.onChange { (_, _, newHeight) =>
          adjustMapSize(newWidth.doubleValue, newHeight.doubleValue)
        }
      }
      
      adjustMapSize(this.width.value, this.height.value)
    }
  }

  /**
    * Adjusts the size of the map image to fit within the available width and height,
    * maintaining the original aspect ratio.
    *
    * @param availableWidth The width available for the map.
    * @param availableHeight The height available for the map.
    */
  private def adjustMapSize(availableWidth: Double, availableHeight: Double): Unit = {
    if (mapImage != null && availableWidth > 0 && availableHeight > 0) {
      val imageRatio = mapImage.width.value / mapImage.height.value
      val containerRatio = availableWidth / availableHeight
      
      if (imageRatio > containerRatio) {
        mapView.fitWidth = availableWidth
        mapView.fitHeight = availableWidth / imageRatio
      } else {
        mapView.fitHeight = availableHeight
        mapView.fitWidth = availableHeight * imageRatio
      }
    }
  }
}
