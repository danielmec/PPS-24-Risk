enum GameAction:
  case TradeCards(territoryCards: Set[TerritoryCard])
  case PlaceTroops(playerId: String, troops: Int)
  case Reinforce(playerId: String, troops: Int)
  case Attack(attackerId: String, defenderId: String)
  case Defend(defenderId: String, troops: Int)
  case EndAttack
  case EndPhase
  case EndTurn