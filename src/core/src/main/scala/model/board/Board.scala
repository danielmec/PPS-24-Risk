package model.board
import model.board.*
import model.player.*

case class Board(
  gameId: String,
  continents: Set[Continent]):

  def territories: Set[Territory] =
    continents.flatMap(_.territories)

  def continentsFullyOwnedBy(player: PlayerState): Set[Continent] =
    continents.filter(_.isFullyOwnedBy(player))

  def territoriesOwnedBy(player: PlayerState): Set[Territory] =
    territories.filter(_.owner.contains(player))

  def areNeighbors(t1: Territory, t2: Territory): Boolean =
    t1.neighbors.contains(t2) 

  def updatedTerritory(newTerritory: Territory): Board =
    copy(continents = continents.map:
      continent =>
        continent.territories.find(_.name == newTerritory.name) match
          case Some(oldTerritory) => continent.copy(territories = continent.territories - oldTerritory + newTerritory)
          case None => continent
    )
