package client

import scalafx.beans.property.{StringProperty, IntegerProperty}
import scalafx.collections.ObservableBuffer

import model.board.{Territory => CoreTerritory, Continent => CoreContinent}
import bridge.TerritoryBridge

/**
 * Adapter per collegare i modelli del core all'UI
 */
object AdapterMap {
  
  /**
   * Classe adapter per Territory che aggiunge proprietà reattive per l'UI
   */
  class UITerritory(coreTerritory: CoreTerritory, val continent: String) {
    val name: String = coreTerritory.name
    val owner = StringProperty(coreTerritory.owner.map(_.id).getOrElse(""))
    val armies = IntegerProperty(coreTerritory.troops)
    
    
    val neighbors: List[String] = coreTerritory.neighbors.map(_.name).toList
    
    
    def isNeighbor(other: UITerritory): Boolean = neighbors.contains(other.name)
    def isNeighbor(territoryName: String): Boolean = neighbors.contains(territoryName)
    
    // aggiorna lo stato dell'UI dal modello del core
    def updateFromCore(updatedTerritory: CoreTerritory): Unit = {
      owner.value = updatedTerritory.owner.map(_.id).getOrElse("")
      armies.value = updatedTerritory.troops
    }
    
    override def toString: String = s"UITerritory($name, $continent, owner=${owner.value}, armies=${armies.value})"
  }
  
  /**
   * Carica tutti i territori dal core e li converte in UITerritory
   */
  def loadTerritories(): ObservableBuffer[UITerritory] = {
    
    val (continents, territoriesMap) = TerritoryBridge.loadTerritoriesAndContinents()
    
    //crea una mappa per associare i territori ai continenti
    val territoryToContinentMap = continents.flatMap(continent => 
      continent.territories.map(territory => territory.name -> continent.name)
    ).toMap
    
    // converte ogni territorio del core in UITerritory
    val uiTerritories = territoriesMap.map { case (name, coreTerritory) =>
      val continentName = territoryToContinentMap.getOrElse(name, "Sconosciuto")
      new UITerritory(coreTerritory, continentName)
    }
    
    ObservableBuffer(uiTerritories.toSeq: _*)
  }
  
  /**
   * Trova un territorio UI dato il nome
   */
  def findTerritory(territories: ObservableBuffer[UITerritory], name: String): Option[UITerritory] = {
    territories.find(_.name == name)
  }
  
  /**
   * Aggiorna un territorio UI con nuove informazioni
   */
  def updateTerritory(territories: ObservableBuffer[UITerritory], name: String, owner: String, armies: Int): Unit = {
    territories.find(_.name == name).foreach { territory =>
      territory.owner.value = owner
      territory.armies.value = armies
    }
  }
}


