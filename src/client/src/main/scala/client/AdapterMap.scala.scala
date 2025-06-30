package client

import scalafx.beans.property.IntegerProperty
import scalafx.beans.property.StringProperty
import scalafx.collections.ObservableBuffer
import model.board.{Territory => CoreTerritory, Continent => CoreContinent}
import bridge.TerritoryBridge

object AdapterMap {
  
  class UITerritory(coreTerritory: CoreTerritory, val continent: String) {
    val name: String = coreTerritory.name
    val owner = StringProperty(coreTerritory.owner.map(_.id).getOrElse(""))
    val armies = IntegerProperty(coreTerritory.troops)
    
    
    val neighbors: List[String] = coreTerritory.neighbors.map(_.name).toList
    
    
    def isNeighbor(other: UITerritory): Boolean = neighbors.contains(other.name)
    def isNeighbor(territoryName: String): Boolean = neighbors.contains(territoryName)
    
    def updateFromCore(updatedTerritory: CoreTerritory): Unit = {
      owner.value = updatedTerritory.owner.map(_.id).getOrElse("")
      armies.value = updatedTerritory.troops
    }
    
    override def toString: String = s"UITerritory($name, $continent, owner=${owner.value}, armies=${armies.value})"
  }
  
  def loadTerritories(): ObservableBuffer[UITerritory] = {
    
    val (continents, territoriesMap) = TerritoryBridge.loadTerritoriesAndContinents()
    
    val territoryToContinentMap = continents.flatMap(continent => 
      continent.territories.map(territory => territory.name -> continent.name)
    ).toMap
    
    val uiTerritories = territoriesMap.map { case (name, coreTerritory) =>
      val continentName = territoryToContinentMap.getOrElse(name, "Sconosciuto")
      new UITerritory(coreTerritory, continentName)
    }
    
    ObservableBuffer(uiTerritories.toSeq: _*)
  }
  
  def findTerritory(territories: ObservableBuffer[UITerritory], name: String): Option[UITerritory] = {
    territories.find(_.name == name)
  }
  
  def updateTerritory(territories: ObservableBuffer[UITerritory], name: String, owner: String, armies: Int): Unit = {
    territories.find(_.name == name).foreach { territory =>
      territory.owner.value = owner
      territory.armies.value = armies
    }
  }
  
  def getNeighbors(territories: ObservableBuffer[UITerritory], territoryName: String): List[String] = {
    territories.find(_.name == territoryName) match {
      case Some(territory) => territory.neighbors
      case None => List.empty
    }
  }
}


