package model.board

import model.player._

/**
  * Represents the game board, containing continents and territories.
  *
  * @param gameId The unique identifier for the game.
  * @param continents The set of continents on the board.
  */
case class Board(
  gameId: String,
  continents: Set[Continent]
):
  /**
    * Returns the list of all territories on the board.
    */
  def territories: List[Territory] = continents.flatMap(_.territories).toList
  
  /**
    * Updates a territory on the board.
    * @param territory The updated territory.
    * @return The updated board.
    */
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
    
  /**
    * Gets a territory by its name.
    * @param name The name of the territory.
    * @return Some(Territory) if found, None otherwise.
    */
  def getTerritoryByName(name: String): Option[Territory] =
    territories.find(_.name == name)
    
  /**
    * Checks if two territories are neighbors.
    * @param t1 The first territory.
    * @param t2 The second territory.
    * @return True if they are neighbors, false otherwise.
    */
  def areNeighbors(t1: Territory, t2: Territory): Boolean =
    t1.neighbors.exists(_.name == t2.name)
    
  /**
    * Returns the list of territories owned by the player with the given ID.
    * @param playerId The ID of the player.
    */
  def territoriesOwnedBy(playerId: String): List[Territory] =
    territories.filter(_.isOwnedBy(playerId))
    
  /**
    * Returns the list of continents fully owned by the player with the given ID.
    * @param playerId The ID of the player.
    */
  def continentsOwnedBy(playerId: String): List[Continent] =
    continents.filter(continent => 
      continent.territories.forall(_.isOwnedBy(playerId))
    ).toList