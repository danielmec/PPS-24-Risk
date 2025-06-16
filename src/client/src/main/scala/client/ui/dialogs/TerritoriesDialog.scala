package client.ui.dialogs

import scalafx.Includes._
import scalafx.geometry.{Insets, Pos}
import scalafx.scene.Scene
import scalafx.scene.control._
import scalafx.scene.control.TabPane.TabClosingPolicy
import scalafx.scene.layout.{BorderPane, HBox}
import scalafx.stage.{Modality, Stage}
import scalafx.collections.ObservableBuffer
import client.ui.models.Territory

/**
 * Finestra di dialogo che mostra le tabelle dei territori organizzati per continente
 */
class TerritoriesDialog(owner: Stage, territories: ObservableBuffer[Territory]) extends Stage {
  
  initOwner(owner)
  initModality(Modality.ApplicationModal)
  title = "Territori di gioco"
  width = 800
  height = 600
  
  //Crea il tab pane
  val tabPane = new TabPane {
    tabClosingPolicy = TabClosingPolicy.Unavailable
  }
  
  // crea una tab per ogni continente
  val continents = List("Nord America", "Sud America", "Europa", "Africa", "Asia", "Australia")
  
  for (continent <- continents) {
    val tab = new Tab {
      text = continent
      closable = false
      
      // tabella per il continente corrente
      val territoryTable = new TableView[Territory] {
        columns ++= List(
          new TableColumn[Territory, String] {
            text = "Territorio"
            cellValueFactory = { cellData =>
              new javafx.beans.property.SimpleStringProperty(cellData.value.name)
            }
            prefWidth = 150
          },
          new TableColumn[Territory, String] {
            text = "Possessore"
            cellValueFactory = { cellData =>
              cellData.value.owner.delegate
            }
            prefWidth = 150
          },
          new TableColumn[Territory, Number] {
            text = "Truppe"
            cellValueFactory = { cellData =>
              cellData.value.armies.delegate
            }
            prefWidth = 100
          }
        )
        
        //filtra i territori per il continente corrente
        items = ObservableBuffer.from(territories.filter(t => t.continentName == continent))
      }
      
      //Layout del contenuto della tab
      content = new BorderPane {
        center = territoryTable
        padding = Insets(10)
      }
    }
    
    tabPane.tabs += tab
  }
  
  // Layout principale
  scene = new Scene(new BorderPane {
    center = tabPane
    bottom = new HBox {
      children = Seq(new Button("Chiudi") {
        onAction = _ => close()
      })
      alignment = Pos.Center
      padding = Insets(10)
    }
  })
}

