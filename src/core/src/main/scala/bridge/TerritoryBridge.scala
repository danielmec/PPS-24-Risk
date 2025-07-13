package bridge

import model.board.{Territory => CoreTerritory, Continent => CoreContinent}
import engine.CardsBuilder
import scala.collection.immutable.{Map => ImmutableMap}

/**
  * Bridge object that allows the client to access territories and continents defined in the core module.
  * Provides methods to load, cache, and query territory and continent data.
  */
object TerritoryBridge {
  
  private var territoriesCache: Option[ImmutableMap[String, CoreTerritory]] = None
  private var continentsCache: Option[Set[CoreContinent]] = None
  
  /**
   * Loads territories from the core module.
   * @return A map of territory names to CoreTerritory objects.
   */
  def loadTerritories(): ImmutableMap[String, CoreTerritory] = {
    territoriesCache match {
      case Some(cache) => cache
      case None =>
        val (continents, territoriesMap) = CardsBuilder.createBoard()
        continentsCache = Some(continents)
        territoriesCache = Some(territoriesMap)
        territoriesMap
    }
  }
  
  /**
   * Loads both territories and continents from the core module.
   * @return A tuple containing the set of continents and the map of territories.
   */
  def loadTerritoriesAndContinents(): (Set[CoreContinent], ImmutableMap[String, CoreTerritory]) = {
    territoriesCache match {
      case Some(_) if continentsCache.isDefined => 
        (continentsCache.get, territoriesCache.get)
      case _ =>
        val (continents, territoriesMap) = CardsBuilder.createBoard()
        continentsCache = Some(continents)
        territoriesCache = Some(territoriesMap)
        (continents, territoriesMap)
    }
  }
  
  /**
   * Gets the names of all territories.
   * @return A sorted list of territory names.
   */
  def getTerritoryNames(): List[String] = {
    loadTerritories().keys.toList.sorted
  }
  
  /**
   * Gets the names of neighboring territories for a given territory.
   * @param territoryName The name of the territory.
   * @return A sorted list of neighboring territory names.
   */
  def getNeighborNames(territoryName: String): List[String] = {
    loadTerritories().get(territoryName) match {
      case Some(territory) => territory.neighbors.map(_.name).toList.sorted
      case None => List.empty
    }
  }
  
  /**
   * Gets the continent to which a territory belongs.
   * @param territoryName The name of the territory.
   * @return The name of the continent, or None if not found.
   */
  def getContinentForTerritory(territoryName: String): Option[String] = {
    val continents = continentsCache.getOrElse {
      val (conts, _) = CardsBuilder.createBoard()
      continentsCache = Some(conts)
      conts
    }
    continents.find(_.territories.exists(_.name == territoryName)).map(_.name)
  }
}