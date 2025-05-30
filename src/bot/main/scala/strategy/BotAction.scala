//import dei territori

enum BotAction:
  case PlaceTroops(territory: Territory, troops: Int)
  case Attack(attacker: Territory, defender: Territory)
  case EndAttack
  case Fortify(from: Territory, to: Territory, troops: Int)
  case EndTurn
