trait Strategy {
  //gamestate è lo stato corrente del gioco
  //Botaction è la mossa che il bot deve eseguire
  //il botplayer è per riferirsi a chi deve decidere la mossa
  def decideMove(gameState: GameState, player: BotPlayer): BotAction
}