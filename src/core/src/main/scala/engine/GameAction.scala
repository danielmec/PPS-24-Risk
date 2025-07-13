package engine

import model.cards.* 

/**
  * Enumeration of all possible actions a player can perform during the game.
  */
enum GameAction:
  /** Trade territory cards for bonus troops. */
  case TradeCards(playerId: String, cardNames: Set[String])
  /** Place troops on a territory. */
  case PlaceTroops(playerId: String, troops: Int, territoryName: String)
  /** Move troops from one territory to another. */
  case Reinforce(playerId: String, from: String, to: String, troops: Int)
  /** Attack another territory. */
  case Attack(attackerId: String, defenderId: String, from: String, to: String, troops: Int)
  /** End the current player's turn. */
  case EndTurn
  /** End the setup phase for the current player. */
  case EndSetup