package engine 

enum TurnPhase: 
    case SetupPlacing
    case WaitingForTurn
    case Reinforcement 
    case PlacingTroops 
    case Attacking
    case Defending