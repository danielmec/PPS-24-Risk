package client.ui.components

import scalafx.Includes._
import scalafx.scene.control.{Label, ScrollPane, Tooltip}
import scalafx.scene.image.{Image, ImageView}
import scalafx.scene.layout.StackPane
import javafx.scene.paint.Color
import scalafx.scene.text.{Font, FontWeight}

/**
 * Componente che gestisce la visualizzazione della mappa di gioco
 */
class GameMapView(onMapClicked: => Unit) extends ScrollPane {
  
  // caricamento dell'immagine 
  val mapImage = try {
    new Image("file:src/client/src/main/scala/client/ui/resources/RiskMap.drawio.png")
  } catch {
    case e: Exception => 
      println(s"Errore nel caricamento della mappa: ${e.getMessage}")
      null
  }
  
  // creazione della vista dell'immagine
  val mapView = new ImageView {
    if (mapImage != null) {
      image = mapImage
      preserveRatio = true
      smooth = true
      
      onMouseClicked = event => onMapClicked
      
      //cambia il cursore quando si passa sopra alla mappa
      cursor = javafx.scene.Cursor.HAND
    }
  }
  
  // Contenitore per la mappa che si adatta alle dimensioni del pannello
  val mapContainer = new StackPane {
    children = Seq(
      if (mapImage != null) mapView else new Label("Mappa non disponibile") {
        font = Font.font("Arial", FontWeight.Bold, 14)
        textFill = Color.RED
      }
    )
    style = "-fx-background-color: #f0f0f0;"
  }
  
  //configurazione del contenuto
  content = mapContainer
  
  // aggiunge un tooltip alla mappa
  val mapTooltip = new Tooltip("Clicca sulla mappa per visualizzare tutti i territori")
  if (mapImage != null) {
    Tooltip.install(mapView, mapTooltip)
  }
  
  // Disabilita le barre di scorrimento
  hbarPolicy = ScrollPane.ScrollBarPolicy.NEVER
  vbarPolicy = ScrollPane.ScrollBarPolicy.NEVER
  fitToWidth = true
  fitToHeight = true
  pannable = false
  
  /**
   * Adatta le dimensioni della mappa in base alla dimensione del pannello contenitore
   * in modo che si adatti completamente e non richieda scroll
   */
  def bindToSceneDimensions(sceneWidth: => Double, sceneHeight: => Double): Unit = {
    if (mapImage != null) {
      
      mapView.fitWidth = 100
      mapView.fitHeight = 100
      
      //listener che ridimensiona la mappa quando il pannello cambia dimensione
      width.onChange { (_, _, newWidth) =>
        height.onChange { (_, _, newHeight) =>
          adjustMapSize(newWidth.doubleValue, newHeight.doubleValue)
        }
      }
      
      //inizializza con le dimensioni attuali
      adjustMapSize(this.width.value, this.height.value)
    }
  }
  
  /**
   * Regola la dimensione della mappa in base allo spazio disponibile
   * mantenendo il rapporto d'aspetto
   */
  private def adjustMapSize(availableWidth: Double, availableHeight: Double): Unit = {
    if (mapImage != null && availableWidth > 0 && availableHeight > 0) {
      val imageRatio = mapImage.width.value / mapImage.height.value
      val containerRatio = availableWidth / availableHeight
      
      if (imageRatio > containerRatio) {
        // L'immagine è più "larga" dello spazio disponibile
        mapView.fitWidth = availableWidth
        mapView.fitHeight = availableWidth / imageRatio
      } else {
        // L'immagine è più "alta" dello spazio disponibile
        mapView.fitHeight = availableHeight
        mapView.fitWidth = availableHeight * imageRatio
      }
    }
  }
}