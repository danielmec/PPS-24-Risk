package client.ui.components

import scalafx.Includes._
import scalafx.geometry.{Insets, Pos}
import scalafx.scene.control.{ComboBox, Label}
import scalafx.scene.layout.{GridPane, HBox, VBox}
import scalafx.scene.text.{Font, FontWeight}
import scalafx.collections.ObservableBuffer

import client.AdapterMap
import client.AdapterMap.UITerritory
import bridge.TerritoryBridge

/**
 * Pannello per visualizzare informazioni sui territori
 */
class TerritoryInfoPane(territories: ObservableBuffer[UITerritory] = null) extends HBox(20) {
  
  // ottiene i territori e i continenti dal bridge
  private val continentToTerritories = {
    val allTerritories = if (territories != null) territories else AdapterMap.loadTerritories() //chiama direttamente il bridge se non sono stati passati territori
    val result = collection.mutable.Map[String, Int]()
    
    // conta i territori per continente
    allTerritories.foreach { territory =>
      val continent = territory.continent
      result(continent) = result.getOrElse(continent, 0) + 1
    }
    
    result.toMap
  }
  
  // Label per mostrare i dettagli del territorio
  private val nameLabel = new Label("--")
  private val armiesLabel = new Label("--")
  private val continentLabel = new Label("--")
  
  // ComboBox per selezionare i territori
  private val territoriesComboBox = new ComboBox[String] {
    items = ObservableBuffer("Seleziona territorio...")
    prefWidth = 200
    onAction = _ => {
      val selectedTerritory = value.value
      if (selectedTerritory != "Seleziona territorio..." && territories != null) {
        territories.find(_.name == selectedTerritory).foreach { territory =>
          updateTerritoryDetails(territory.name, territory.armies.value, territory.continent)
        }
      }
    }
  }
  
  // Costruisci la griglia dei continenti dinamicamente
  private val continentsGrid = new GridPane {
    hgap = 10
    vgap = 5
    
    var row = 0
    for ((continent, count) <- continentToTerritories.toSeq.sortBy(_._1)) {
      add(new Label(s"$continent:"), 0, row)
      add(new Label(s"0/$count"), 1, row)
      row += 1
    }
  }
  
  // Inizializza l'UI
  padding = Insets(10)
  alignment = Pos.CenterLeft
  style = "-fx-background-color: #f5f5f5; -fx-border-color: #aaaaaa; -fx-border-width: 1 0 1 0;"
  minHeight = 100
  
  children = Seq(
    // ComboBox per selezionare territori del giocatore
    new VBox(5) {
      alignment = Pos.CenterLeft
      children = Seq(
        new Label("I miei territori:") {
          font = Font.font("Arial", FontWeight.Bold, 12)
        },
        territoriesComboBox
      )
    },
    
    // Pannello statistiche del territorio selezionato
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
          add(nameLabel, 1, 0)
          add(new Label("Armate:"), 0, 1)
          add(armiesLabel, 1, 1)
          add(new Label("Continente:"), 0, 2)
          add(continentLabel, 1, 2)
        }
      )
    },
    
    // Panoramica continenti (creata dinamicamente in base ai dati effettivi)
    new VBox(5) {
      alignment = Pos.CenterLeft
      children = Seq(
        new Label("Controllo continenti:") {
          font = Font.font("Arial", FontWeight.Bold, 12)
        },
        continentsGrid
      )
    }
  )
  
  /**
   * Aggiorna i dettagli del territorio selezionato
   */
  def updateTerritoryDetails(name: String, armies: Int, continent: String): Unit = {
    nameLabel.text = name
    armiesLabel.text = armies.toString
    continentLabel.text = continent
  }
  
  /**
   * Aggiorna la combobox con i territori di un giocatore
   */
  def updatePlayerTerritories(playerName: String): Unit = {
    if (territories != null) {
      val playerTerritories = territories.filter(_.owner.value == playerName).map(_.name).toSeq.sorted
      territoriesComboBox.items = ObservableBuffer("Seleziona territorio...") ++ playerTerritories
      territoriesComboBox.selectionModel().select(0)
    }
  }
  
  /**
   * Aggiorna il controllo dei continenti
   * @param playerName Nome del giocatore
   */
  def updateContinentControl(playerName: String): Unit = {
    if (territories != null) {
      
      val territoriesByContinent = territories.groupBy(_.continent)
      
      // per ogni continente, conta quanti territori sono posseduti dal giocatore
      continentToTerritories.keys.foreach { continent =>
        val total = continentToTerritories(continent)
        val owned = territoriesByContinent.getOrElse(continent, Seq.empty)
                                         .count(_.owner.value == playerName)
                                         
        // la label corrispondente nel grid e aggiorna
        val rowIndex = continentToTerritories.keys.toSeq.sorted.indexOf(continent)
        if (rowIndex >= 0) {
          val node = continentsGrid.getChildren.find(node => 
            GridPane.getRowIndex(node) == rowIndex && GridPane.getColumnIndex(node) == 1
          )
          
          node.foreach { label =>
            label.asInstanceOf[javafx.scene.control.Label].setText(s"$owned/$total")
          }
        }
      }
    }
  }
}

