case class Board(
  gameId: String,
  continents: Set[Continent]):

  def territories: Set[Territory] =
    continents.flatMap(_.territories)

  def continentsFullyOwnedBy(player: Player): Set[Continent] =
    continents.filter(_.isControlledBy(player))

  def territoriesOwnedBy(player: Player): Set[Territory] =
    territories.filter(_.owner.contains(player))

  def areNeighbors(t1: Territory, t2: Territory): Boolean =
    t1.neighbors.contains(t2.name) 

  def updatedTerritory(newTerritory: Territory): Board =
    copy(continents = continents.map:
      continent =>
        continent.territories.find(_.name == newTerritory.name) match
          case Some(oldTerritory) => continent.copy(territories = continent.territories - oldTerritory + newTerritory)
          case None => continent
    )
