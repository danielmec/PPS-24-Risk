trait Strategy {
  //gamestate è lo stato corrente del gioco
  //Botaction è la mossa che il bot deve eseguire
  def decideMove(gameState: GameState): List[BotAction]
}