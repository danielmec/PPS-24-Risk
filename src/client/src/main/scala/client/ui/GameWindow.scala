package client.ui

import scalafx.Includes._
import scalafx.application.Platform
import scalafx.geometry.{Insets, Pos, Orientation}
import scalafx.scene.Scene
import scalafx.scene.control._
import scalafx.scene.layout._
import scalafx.scene.text._
import scalafx.stage.Stage
import scalafx.collections.ObservableBuffer
import scala.math.Numeric.Implicits.infixNumericOps 

import client.ClientNetworkManager
import client.ClientJsonSupport._
import client.ui.models.Territory
import client.ui.components._
import client.ui.dialogs.TerritoriesDialog

class GameWindow(
  networkManager: ClientNetworkManager,
  val gameId: String,
  gameName: String, 
  initPlayers: List[String]
) extends Stage {
  
  title = s"Risk - Partita: $gameName"
  width = 1100
  height = 800
  
  //Inizializzazione dei territori
  val territories = createTerritories()
  
  
  val titleLabel = new Label(s"Partita: $gameName")
  titleLabel.font = Font.font("Arial", FontWeight.Bold, 18)
  
  val playersLabel = new Label(s"Giocatori: ${initPlayers.size}")
  
  val topPane = new VBox(10) {
    padding = Insets(10)
    children = Seq(titleLabel, playersLabel)
  }
  
  //creazione componenti
  def showTerritoriesDialog(): Unit = {
    val dialog = new TerritoriesDialog(this, territories)
    dialog.showAndWait()
  }
  
  val gameMapView = new GameMapView(showTerritoriesDialog())
  val territoryInfoPane = new TerritoryInfoPane()
  val actionPane = new ActionPane(showTerritoriesDialog())
  
  
  val centerPane = new VBox {
    children = Seq(gameMapView, territoryInfoPane, actionPane)
    VBox.setVgrow(gameMapView, Priority.Always)
  }
  
  // Barra laterale
  val playersList = ObservableBuffer(initPlayers.map(name => new PlayerInfoView(name, initPlayers, networkManager)): _*)
  val diceDisplay = new DiceDisplay()
  
  val sidebarPane = createSidebarPane()
  
 
  val splitPane = new SplitPane {
    orientation = Orientation.Horizontal
    items.addAll(centerPane, sidebarPane)
    dividerPositions = 0.75 // 75% mappa, 25% barra laterale
    VBox.setVgrow(this, Priority.Always)
  }
  
  // Barra inferiore
  val leaveButton = new Button("Abbandona partita")
  leaveButton.onAction = handle {
    networkManager.sendMessage(LeaveGameMessage(gameId)).onComplete {
      case _ => Platform.runLater {
        close()
      }
    }(networkManager.executionContext)
  }
  
  val bottomPane = new HBox(10) {
    padding = Insets(10)
    alignment = Pos.CenterRight
    children = Seq(leaveButton)
  }
  
  // Layout principale
  val root = new BorderPane()
  root.top = topPane
  root.center = splitPane
  root.bottom = bottomPane
  
  scene = new Scene(root)
  
  // Binding delle dimensioni - usando doubleValue() invece di toDouble
  gameMapView.bindToSceneDimensions(scene.width.value.doubleValue(), scene.height.value.doubleValue())
  
  /**
   * Aggiorna la lista dei giocatori nella partita
   */
  def updatePlayers(newPlayers: List[String]): Unit = {
    Platform.runLater {
      // Aggiorna il titolo della finestra con il numero di giocatori
      playersLabel.text = s"Giocatori: ${newPlayers.size}"
      
      // Ricrea la struttura dei giocatori
      val updatedPlayersList = ObservableBuffer(newPlayers.map(name => 
        new PlayerInfoView(name, newPlayers, networkManager)): _*)
      
      // Aggiorna la sidebar
      val newSidebarPane = createSidebarPane(updatedPlayersList)
      splitPane.items.set(1, newSidebarPane)
      
      // Aggiorna i riferimenti
      playersList.clear()
      playersList.addAll(updatedPlayersList)
      
      println(s"Lista giocatori ricreata con: ${newPlayers.mkString(", ")}")
    }
  }
  
  /**
   * Aggiorna le informazioni sui territori
   */
  def updateTerritory(territoryName: String, owner: String, armies: Int): Unit = {
    Platform.runLater {
      territories.find(_.name == territoryName).foreach { territory =>
        territory.owner.value = owner
        territory.armies.value = armies
      }
    }
  }
  
  /**
   * Aggiorna i valori dei dadi visualizzati
   */
  def updateDiceValues(attackerValues: List[Int], defenderValues: List[Int]): Unit = {
    diceDisplay.updateValues(attackerValues, defenderValues)
  }
  
  //Metodi di supporto
  private def createSidebarPane(players: ObservableBuffer[PlayerInfoView] = playersList): VBox = {
    new VBox(15) {
      padding = Insets(10)
      style = "-fx-background-color: #e8e8e8;"
      
      children = Seq(
        new Label("Giocatori") {
          font = Font.font("Arial", FontWeight.Bold, 16)
          textAlignment = TextAlignment.Center
          alignment = Pos.Center
          maxWidth = Double.MaxValue
        },
        new Separator(),
        new VBox(10) {
          children = players
          VBox.setVgrow(this, Priority.Always)
        },
        new Separator(),
        new Label("Dadi") {
          font = Font.font("Arial", FontWeight.Bold, 16)
          textAlignment = TextAlignment.Center
          alignment = Pos.Center
          maxWidth = Double.MaxValue
        },
        diceDisplay
      )
      
      maxWidth = 250
      prefWidth = 250
    }
  }
  
  private def createTerritories(): ObservableBuffer[Territory] = {
    ObservableBuffer[Territory](
      // Nord America
      new Territory("Alaska", "Nord America"),
      new Territory("Alberta", "Nord America"),
      new Territory("America Centrale", "Nord America"),
      new Territory("Stati Uniti Orientali", "Nord America"),
      new Territory("Groenlandia", "Nord America"),
      new Territory("Territori del Nord-Ovest", "Nord America"),
      new Territory("Ontario", "Nord America"),
      new Territory("Quebec", "Nord America"),
      new Territory("Stati Uniti Occidentali", "Nord America"),
      
      // Sud America
      new Territory("Argentina", "Sud America"),
      new Territory("Brasile", "Sud America"),
      new Territory("Perù", "Sud America"),
      new Territory("Venezuela", "Sud America"),
      
      // Europa
      new Territory("Gran Bretagna", "Europa"),
      new Territory("Islanda", "Europa"),
      new Territory("Europa Settentrionale", "Europa"),
      new Territory("Scandinavia", "Europa"),
      new Territory("Europa Meridionale", "Europa"),
      new Territory("Ucraina", "Europa"),
      new Territory("Europa Occidentale", "Europa"),
      
      // Africa
      new Territory("Congo", "Africa"),
      new Territory("Africa Orientale", "Africa"),
      new Territory("Egitto", "Africa"),
      new Territory("Madagascar", "Africa"),
      new Territory("Nord Africa", "Africa"),
      new Territory("Sud Africa", "Africa"),
      
      // Asia
      new Territory("Afghanistan", "Asia"),
      new Territory("Cina", "Asia"),
      new Territory("India", "Asia"),
      new Territory("Irkutsk", "Asia"),
      new Territory("Giappone", "Asia"),
      new Territory("Kamchatka", "Asia"),
      new Territory("Medio Oriente", "Asia"),
      new Territory("Mongolia", "Asia"),
      new Territory("Siam", "Asia"),
      new Territory("Siberia", "Asia"),
      new Territory("Ural", "Asia"),
      new Territory("Yakutsk", "Asia"),
      
      // Australia
      new Territory("Australia Orientale", "Australia"),
      new Territory("Indonesia", "Australia"),
      new Territory("Nuova Guinea", "Australia"),
      new Territory("Australia Occidentale", "Australia")
    )
  }
}