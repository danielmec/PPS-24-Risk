package engine 

enum TurnPhase: 
  case SetupPhase   // Solo per il piazzamento iniziale
  case MainPhase    // Fase principale di gioco (piazzamento truppe + attacchi + rinforzi)