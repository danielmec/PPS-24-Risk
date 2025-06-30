package exceptions

import model.player._

class GameOverException(winner: PlayerState) extends Exception(
    s"Game over! Player ${winner.player.name} (${winner.player.color}) has won the game! Objective completed: ${winner.objectiveCard.map(_.description).getOrElse("N/D")}"
)