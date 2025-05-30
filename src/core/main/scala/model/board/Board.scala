case class Board(
  gameId: String,
  continents: Set[Continent]):

  def territories: Set[Territory] =
    continents.flatMap(_.territories)

  def continentsFullyOwnedBy(player: Player): Set[Continent] =
    continents.filter(_.isControlledBy(player))
