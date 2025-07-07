package engine

import model.cards.* 

enum GameAction:
  case TradeCards(playerId: String, cardNames: Set[String])
  case PlaceTroops(playerId: String, troops: Int, territoryName: String)
  case Reinforce(playerId: String, from: String, to: String, troops: Int)
  case Attack(attackerId: String, defenderId: String, from: String, to: String, troops: Int)
  case EndTurn
  case EndSetup