package bridge

import model.board.{Territory => CoreTerritory, Continent => CoreContinent}
import engine.CardsBuilder
import scala.collection.immutable.{Map => ImmutableMap}  // Usa Map immutabile

/**
 * Bridge che permette al client di accedere ai territori definiti nel core
 */
object TerritoryBridge {
  
  
  private var territoriesCache: Option[ImmutableMap[String, CoreTerritory]] = None
  private var continentsCache: Option[Set[CoreContinent]] = None
  
  /**
   * Carica i territori dal core
   * @return Mappa dei territori con il nome come chiave
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
   * Carica i territori e i continenti dal core
   * @return Tuple con il set di continenti e la mappa dei territori 
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
   * Ottiene tutti i nomi dei territori
   * @return Lista dei nomi dei territori
   */
  def getTerritoryNames(): List[String] = {
    loadTerritories().keys.toList.sorted
  }
  
  /**
   * Ottiene i nomi dei territori adiacenti a un territorio dato
   * @param territoryName Nome del territorio
   * @return Lista dei nomi dei territori adiacenti
   */
  def getNeighborNames(territoryName: String): List[String] = {
    loadTerritories().get(territoryName) match {
      case Some(territory) => territory.neighbors.map(_.name).toList.sorted
      case None => List.empty
    }
  }
  
  /**
   * Ottiene il continente a cui appartiene un territorio
   * @param territoryName Nome del territorio
   * @return Nome del continente, o None se non trovato
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