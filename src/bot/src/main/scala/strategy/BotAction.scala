package strategy

import model.board.Territory
import model.cards.*

enum BotAction:
  case TradeCards(territoryCards: Set[TerritoryCard])
  case PlaceTroops(territory: Territory, troops: Int)
  case Attack(from: Territory, to: Territory, troops: Int)
  case MoveTroops(from: Territory, to: Territory, troops: Int)
  case EndPhase
  case EndTurn