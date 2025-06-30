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


class GameMapView(onMapClicked: => Unit) extends ScrollPane {
  
  val mapImage = try {
    new Image("file:src/client/src/main/scala/client/ui/resources/RiskMap.drawio.png")
  } catch {
    case e: Exception => 
      println(s"Errore nel caricamento della mappa: ${e.getMessage}")
      null
  }
  
  val mapView = new ImageView {
    if (mapImage != null) {
      image = mapImage
      preserveRatio = true
      smooth = true
      
      onMouseClicked = event => onMapClicked
      
      cursor = javafx.scene.Cursor.HAND
    }
  }
  
  val mapContainer = new StackPane {
    children = Seq(
      if (mapImage != null) mapView else new Label("Mappa non disponibile") {
        font = Font.font("Arial", FontWeight.Bold, 14)
        textFill = Color.RED
      }
    )
    style = "-fx-background-color: #f0f0f0;"
  }
  
  content = mapContainer
  
  val mapTooltip = new Tooltip("Clicca sulla mappa per visualizzare tutti i territori")
  if (mapImage != null) {
    Tooltip.install(mapView, mapTooltip)
  }

  hbarPolicy = ScrollPane.ScrollBarPolicy.NEVER
  vbarPolicy = ScrollPane.ScrollBarPolicy.NEVER
  fitToWidth = true
  fitToHeight = true
  pannable = false
  
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