package client.ui.components

import scalafx.Includes._
import scalafx.geometry.{Insets, Pos}
import scalafx.scene.control.{ComboBox, Label}
import scalafx.scene.layout.{GridPane, HBox, VBox}
import scalafx.scene.text.{Font, FontWeight}
import scalafx.collections.ObservableBuffer

/**
 * Pannello per visualizzare informazioni sui territori
 */
class TerritoryInfoPane extends HBox(20) {
  
  padding = Insets(10)
  alignment = Pos.CenterLeft
  style = "-fx-background-color: #f5f5f5; -fx-border-color: #aaaaaa; -fx-border-width: 1 0 1 0;"
  minHeight = 100
  
  children = Seq(
    //Combo box per selezionare territori del giocatore
    new VBox(5) {
      alignment = Pos.CenterLeft
      children = Seq(
        new Label("I miei territori:") {
          font = Font.font("Arial", FontWeight.Bold, 12)
        },
        new ComboBox[String] {
          items = ObservableBuffer("Seleziona territorio...") // Verrà popolato durante il gioco
          prefWidth = 200
          onAction = _ => {
            //  dettagli sul territorio selezionato
          }
        }
      )
    },
    
    // pannello statistiche del territorio selezionato
    new VBox(5) {
      alignment = Pos.CenterLeft
      minWidth = 200
      children = Seq(
        new Label("Dettagli territorio:") {
          font = Font.font("Arial", FontWeight.Bold, 12)
        },
        new GridPane {
          hgap = 10
          vgap = 5
          add(new Label("Nome:"), 0, 0)
          add(new Label("--"), 1, 0)
          add(new Label("Armate:"), 0, 1)
          add(new Label("--"), 1, 1)
          add(new Label("Continente:"), 0, 2)
          add(new Label("--"), 1, 2)
        }
      )
    },
    
    // Panoramica continenti
    new VBox(5) {
      alignment = Pos.CenterLeft
      children = Seq(
        new Label("Controllo continenti:") {
          font = Font.font("Arial", FontWeight.Bold, 12)
        },
        new GridPane {
          hgap = 10
          vgap = 5
          add(new Label("Europa:"), 0, 0)
          add(new Label("0/7"), 1, 0)
          add(new Label("America N.:"), 0, 1)
          add(new Label("0/9"), 1, 1)
          add(new Label("Asia:"), 0, 2)
          add(new Label("0/12"), 1, 2)
        }
      )
    }
  )
  
  /**
   * Aggiorna i dettagli del territorio selezionato
   */
  def updateTerritoryDetails(name: String, armies: Int, continent: String): Unit = {
    //da fare
  }
}

