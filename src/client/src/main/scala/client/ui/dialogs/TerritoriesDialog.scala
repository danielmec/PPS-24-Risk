package client.ui.dialogs

import scalafx.Includes._
import scalafx.geometry.Insets
import scalafx.geometry.Pos
import scalafx.scene.Scene
import scalafx.scene.control._
import scalafx.scene.control.TabPane.TabClosingPolicy
import scalafx.scene.layout.BorderPane
import scalafx.scene.layout.VBox
import scalafx.scene.layout.HBox
import scalafx.stage.Modality
import scalafx.stage.Stage
import scalafx.collections.ObservableBuffer
import scalafx.beans.property.StringProperty
import client.AdapterMap.UITerritory

class TerritoriesDialog(
    owner: Stage, 
    territories: ObservableBuffer[UITerritory],
    playerIdToUsername: Map[String, String] = Map.empty
) extends Stage {
  
  initOwner(owner)
  initModality(Modality.ApplicationModal)
  title = "Territori di gioco"
  width = 900
  height = 600
  
  val tabPane = new TabPane {
    tabClosingPolicy = TabClosingPolicy.Unavailable
  }
  
  val continents = territories.map(_.continent).distinct.sorted.toList
  
  //listener ai territori per rimanere aggiornati sui cambi di proprietà
  val territoryTables = scala.collection.mutable.Map[String, TableView[UITerritory]]()
  
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
            text = "ID Proprietario"
            cellValueFactory = { cellData =>
              //legge direttamente l'ID dalla proprietà owner di UITerritory
              cellData.value.owner
            }
            prefWidth = 120
          },
          new TableColumn[UITerritory, String] {
            text = "Username"
            cellValueFactory = { cellData =>
              //crea una StringProperty che si aggiorna automaticamente
              // quando cambia il proprietario del territorio
              val usernameProperty = new StringProperty()
              
              //funzione che aggiorna lo username in base all'ID
              def updateUsername(): Unit = {
                val playerId = cellData.value.owner.value
                val username = if (playerId.nonEmpty) {
                  playerIdToUsername.getOrElse(playerId, "Sconosciuto")
                } else {
                  ""
                }
                usernameProperty.value = username
              }
              //inizializza lo username all'apertura della tabella
              updateUsername()
              
              //listener che aggiorna lo username quando cambia il proprietario
              cellData.value.owner.onChange { (_, _, newValue) =>
                updateUsername()
              }
              
              usernameProperty
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
      
      territoryTables(continent) = territoryTable
    }
    
    tabPane.tabs += tab
  }
  
  scene = new Scene(new BorderPane {
    center = tabPane
    bottom = new HBox {
      children = Seq(
        new Button("Aggiorna") {
          onAction = _ => {

            territoryTables.values.foreach { table =>
              table.refresh()
            }
          }
        },
        new Button("Chiudi") {
          onAction = _ => close()
        }
      )
      spacing = 10
      alignment = Pos.Center
      padding = Insets(10)
    }
  })
}

