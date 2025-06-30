package client.ui.dialogs

import scalafx.Includes._
import scalafx.geometry.{Insets, Pos}
import scalafx.scene.Scene
import scalafx.scene.control._
import scalafx.scene.control.TabPane.TabClosingPolicy
import scalafx.scene.layout.{BorderPane, HBox}
import scalafx.stage.{Modality, Stage}
import scalafx.collections.ObservableBuffer
import client.AdapterMap.UITerritory

class TerritoriesDialog(owner: Stage, territories: ObservableBuffer[UITerritory]) extends Stage {
  
  initOwner(owner)
  initModality(Modality.ApplicationModal)
  title = "Territori di gioco"
  width = 800
  height = 600
  
  val tabPane = new TabPane {
    tabClosingPolicy = TabClosingPolicy.Unavailable
  }
  
  val continents = territories.map(_.continent).distinct.sorted.toList
  
  for (continent <- continents) {
    val tab = new Tab {
      text = continent
      closable = false
      
      val territoryTable = new TableView[UITerritory] {
        columns ++= List(
          new TableColumn[UITerritory, String] {
            text = "Territorio"
            cellValueFactory = { cellData =>
              new javafx.beans.property.SimpleStringProperty(cellData.value.name)
            }
            prefWidth = 150
          },
          new TableColumn[UITerritory, String] {
            text = "Possessore"
            cellValueFactory = { cellData =>
              cellData.value.owner.delegate
            }
            prefWidth = 150
          },
          new TableColumn[UITerritory, Number] {
            text = "Truppe"
            cellValueFactory = { cellData =>
              cellData.value.armies.delegate
            }
            prefWidth = 100
          }
        )
        
        items = ObservableBuffer.from(territories.filter(t => t.continent == continent))
      }
      
      content = new BorderPane {
        center = territoryTable
        padding = Insets(10)
      }
    }
    
    tabPane.tabs += tab
  }
  
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

