package client.ui.components

import scalafx.Includes._
import scalafx.geometry.Insets
import scalafx.geometry.Pos
import scalafx.scene.control.ComboBox
import scalafx.scene.control.Label
import scalafx.scene.layout.HBox
import scalafx.scene.layout.GridPane
import scalafx.scene.layout.VBox
import scalafx.scene.text.FontWeight
import scalafx.scene.text.Font 
import scalafx.collections.ObservableBuffer
import client.AdapterMap
import client.AdapterMap.UITerritory
import bridge.TerritoryBridge

/**
  * A UI component that displays information about territories, including:
  * - A combo box for selecting player's territories,
  * - Details of the selected territory (name, armies, continent),
  * - A grid showing continent control status (owned territories count / total territories).
  *
  * @param territories An observable buffer of UITerritory instances representing the territories. 
  *                    If null, all territories will be loaded from AdapterMap.
  */
class TerritoryInfoPane(territories: ObservableBuffer[UITerritory] = null) extends HBox(20) {
  
  /**
    * A map of continents to the total number of territories in each continent.
    */
  private val continentToTerritories = {
    val allTerritories = if (territories != null) territories else AdapterMap.loadTerritories()
    val result = collection.mutable.Map[String, Int]()
    
    allTerritories.foreach { territory =>
      val continent = territory.continent
      result(continent) = result.getOrElse(continent, 0) + 1
    }
    
    result.toMap
  }
  
  private val nameLabel = new Label("--")
  private val armiesLabel = new Label("--")
  private val continentLabel = new Label("--")
  
  /**
    * ComboBox allowing the player to select one of their territories.
    * Updates the territory details on selection.
    */
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
  
  /**
    * A GridPane showing continent names with the count of owned territories over total territories.
    */
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
  
  padding = Insets(10)
  alignment = Pos.CenterLeft
  style = "-fx-background-color: #f5f5f5; -fx-border-color: #aaaaaa; -fx-border-width: 1 0 1 0;"
  minHeight = 100
  
  children = Seq(
    new VBox(5) {
      alignment = Pos.CenterLeft
      children = Seq(
        new Label("I miei territori:") {
          font = Font.font("Arial", FontWeight.Bold, 12)
        },
        territoriesComboBox
      )
    },
    
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
    * Updates the territory detail labels with the given name, armies count, and continent.
    * 
    * @param name The name of the territory
    * @param armies The number of armies stationed in the territory
    * @param continent The continent to which the territory belongs
    */
  def updateTerritoryDetails(name: String, armies: Int, continent: String): Unit = {
    nameLabel.text = name
    armiesLabel.text = armies.toString
    continentLabel.text = continent
  }
  
  /**
    * Updates the combo box with territories owned by the specified player.
    *
    * @param playerName The name of the player whose territories will be displayed
    */
  def updatePlayerTerritories(playerName: String): Unit = {
    if (territories != null) {
      val playerTerritories = territories.filter(_.owner.value == playerName).map(_.name).toSeq.sorted
      territoriesComboBox.items = ObservableBuffer("Seleziona territorio...") ++ playerTerritories
      territoriesComboBox.selectionModel().select(0)
    }
  }
  
  /**
    * Updates the continent control grid with the count of territories owned by the player for each continent.
    *
    * @param playerName The name of the player whose continent control will be updated
    */
  def updateContinentControl(playerName: String): Unit = {
    if (territories != null) {
      
      val territoriesByContinent = territories.groupBy(_.continent)
      
      continentToTerritories.keys.foreach { continent =>
        val total = continentToTerritories(continent)
        val owned = territoriesByContinent.getOrElse(continent, Seq.empty).count(_.owner.value == playerName)
                                         
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
