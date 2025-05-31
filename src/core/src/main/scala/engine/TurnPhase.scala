package engine 

enum TurnPhase: 
    case WaitingForTurn
    case Reinforcement 
    case PlacingTroops 
    case Attacking
    case Defending