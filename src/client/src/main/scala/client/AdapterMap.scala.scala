package client

import scalafx.beans.property.IntegerProperty
import scalafx.beans.property.StringProperty
import scalafx.collections.ObservableBuffer
import model.board.{Territory => CoreTerritory, Continent => CoreContinent}
import bridge.TerritoryBridge

/**
 * AdapterMap provides utilities for adapting core game data structures to UI-friendly representations.
 *
 * This object contains:
 * - The UITerritory class, which wraps a core Territory and exposes observable properties for UI binding.
 * - Utility methods for loading territories, finding and updating them, and retrieving neighbors.
 *
 * Main features:
 * - Converts core model territories and continents into UI objects with observable properties.
 * - Supports territory lookup and updates for UI synchronization.
 * - Provides neighbor information for UI logic (e.g., attack/reinforcement validation).
 */
object AdapterMap {

  /**
   * UITerritory is a UI-friendly wrapper for a core Territory.
   *
   * It exposes observable properties for owner and armies, making it suitable for use in ScalaFX views.
   * Also provides methods for neighbor checks and updating from the core model.
   *
   * @param coreTerritory The core Territory object to wrap
   * @param continent The name of the continent this territory belongs to
   */
  class UITerritory(coreTerritory: CoreTerritory, val continent: String) {
    /** Name of the territory. */
    val name: String = coreTerritory.name
    /** Observable property for the owner ID. */
    val owner = StringProperty(coreTerritory.owner.map(_.id).getOrElse(""))
    /** Observable property for the number of armies. */
    val armies = IntegerProperty(coreTerritory.troops)
    /** List of neighboring territory names. */
    val neighbors: List[String] = coreTerritory.neighbors.map(_.name).toList

    /**
     * Checks if another UITerritory is a neighbor.
     * @param other The other UITerritory to check
     * @return true if neighbor, false otherwise
     */
    def isNeighbor(other: UITerritory): Boolean = neighbors.contains(other.name)

    /**
     * Checks if a territory name is a neighbor.
     * @param territoryName The name of the territory to check
     * @return true if neighbor, false otherwise
     */
    def isNeighbor(territoryName: String): Boolean = neighbors.contains(territoryName)

    /**
     * Updates this UITerritory's owner and armies from a core Territory.
     * @param updatedTerritory The updated core Territory
     */
    def updateFromCore(updatedTerritory: CoreTerritory): Unit = {
      owner.value = updatedTerritory.owner.map(_.id).getOrElse("")
      armies.value = updatedTerritory.troops
    }

    override def toString: String = s"UITerritory($name, $continent, owner=${owner.value}, armies=${armies.value})"
  }

  /**
   * Loads all territories and continents from the core model and adapts them for UI use.
   *
   * @return ObservableBuffer of UITerritory objects for UI binding
   */
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

  /**
   * Finds a UITerritory by name in the given buffer.
   *
   * @param territories Buffer of UITerritory objects
   * @param name Name of the territory to find
   * @return Option[UITerritory] if found, None otherwise
   */
  def findTerritory(territories: ObservableBuffer[UITerritory], name: String): Option[UITerritory] = {
    territories.find(_.name == name)
  }

  /**
   * Updates the owner and armies of a UITerritory in the buffer.
   *
   * @param territories Buffer of UITerritory objects
   * @param name Name of the territory to update
   * @param owner New owner ID
   * @param armies New number of armies
   */
  def updateTerritory(territories: ObservableBuffer[UITerritory], name: String, owner: String, armies: Int): Unit = {
    territories.find(_.name == name).foreach { territory =>
      territory.owner.value = owner
      territory.armies.value = armies
    }
  }

  /**
   * Gets the list of neighbor names for a given territory.
   *
   * @param territories Buffer of UITerritory objects
   * @param territoryName Name of the territory whose neighbors to retrieve
   * @return List of neighbor territory names, or empty list if not found
   */
  def getNeighbors(territories: ObservableBuffer[UITerritory], territoryName: String): List[String] = {
    territories.find(_.name == territoryName) match {
      case Some(territory) => territory.neighbors
      case None => List.empty
    }
  }
}


