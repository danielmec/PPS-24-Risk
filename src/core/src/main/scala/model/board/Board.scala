package model.board
import model.board.*
import model.player.*

case class Board(
  gameId: String,
  continents: Set[Continent]
):
  def territories: List[Territory] = continents.flatMap(_.territories).toList
  
  def updatedTerritory(territory: Territory): Board =
    val updatedContinents = continents.map { continent =>
      if continent.territories.exists(_.name == territory.name) then
        continent.copy(territories = continent.territories.map { t =>
          if t.name == territory.name then territory else t
        })
      else continent
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