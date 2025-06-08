package engine
import model.cards.* 

enum GameAction:
  case TradeCards(territoryCards: Set[TerritoryCard])
  case PlaceTroops(playerId: String, troops: Int, territoryName: String)
  case Reinforce(playerId: String, from: String, to: String, troops: Int)
  case Attack(attackerId: String, defenderId: String, from: String, to: String, troops: Int)
  case Defend(defenderId: String, territory: String, troops: Int)
  case EndAttack
  case EndPhase
  case EndTurn