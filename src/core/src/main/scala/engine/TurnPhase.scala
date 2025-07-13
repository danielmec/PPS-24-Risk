package engine 

/**
  * Enumeration representing the phases of a turn in the game.
  */
enum TurnPhase: 
  /** The setup phase, where players place their initial troops. */
  case SetupPhase  
  /** The main phase, where players perform actions such as attacking and reinforcing. */
  case MainPhase