package model.board

import model.player._

case class Board(
  gameId: String,
  continents: Set[Continent]
):
  def territories: List[Territory] = continents.flatMap(_.territories).toList
  
  def updatedTerritory(territory: Territory): Board =
    val updatedContinents = continents.map {
      case continent if continent.territories.exists(_.name == territory.name) =>
        continent.copy(territories = continent.territories.map {
          case t if t.name == territory.name => territory
          case t => t
        })
      case continent => continent
    }
    copy(continents = updatedContinents)
    
  def getTerritoryByName(name: String): Option[Territory] =
    territories.find(_.name == name)
    
  def areNeighbors(t1: Territory, t2: Territory): Boolean =
    t1.neighbors.exists(_.name == t2.name)
    
  def territoriesOwnedBy(playerId: String): List[Territory] =
    territories.filter(_.isOwnedBy(playerId))
    
  def continentsOwnedBy(playerId: String): List[Continent] =
    continents.filter(continent => 
      continent.territories.forall(_.isOwnedBy(playerId))
    ).toList