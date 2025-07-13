package utils

import model.board._
import model.cards._
import model.player._
import engine._

/**
 * Utility object for calculating various types of bonuses in the game.
 */
object BonusCalculator:

  /**
   * Calculates the troop bonus for trading in a set of three territory cards.
   *
   * @param cards A sequence of three territory cards played by the player.
   * @return The number of bonus troops awarded based on the card combination:
   *         - 4 for three Artillery cards
   *         - 6 for three Infantry cards
   *         - 8 for three Cavalry cards
   *         - 10 for one of each type (mixed set)
   *         - 0 for any invalid set (e.g., wrong number of cards or unrecognized combination)
   */
  def calculateTradeBonus(cards: Seq[TerritoryCard]): Int =
    val imgs = cards.map(_.cardImg)
    println(s"[DEBUG] Calcolo tris per: $imgs")
    if (imgs.size != 3) 0
    else if (imgs.forall(_ == CardImg.Artillery)) 4
    else if (imgs.forall(_ == CardImg.Infantry)) 6
    else if (imgs.forall(_ == CardImg.Cavalry)) 8
    else if (imgs.distinct.size == 3) 10
    else 0

  /**
   * Calculates the start-of-turn troop bonus for a given player.
   *
   * @param playerId The ID of the player.
   * @param board The current game board state.
   * @return The total bonus troops based on:
   *         - Number of territories owned (minimum of 3 troops, or territoriesOwned / 3)
   *         - Additional bonus troops from owning entire continents
   */
  def calculateStartTurnBonus(playerId: String, board: Board): Int =
    val territoriesOwned = board.territoriesOwnedBy(playerId).size
    val continentsOwned = board.continentsOwnedBy(playerId)
    val territoryBonus = math.max(3, territoriesOwned / 3)
    val continentBonus = continentsOwned.map(_.bonusTroops).sum  
    territoryBonus + continentBonus

  /**
   * Initializes the number of troops each player has at the start of the game,
   * based on the number of players and territories already owned.
   *
   * @param players The list of participating players.
   * @param playerStates The list of current player states.
   * @param board The current game board.
   * @return A new list of player states with initial troops assigned and the turn phase
   *         set to `SetupPhase`.
   */
  def calculateInitialTroops(players: List[PlayerImpl], playerStates: List[PlayerState], board: Board): List[PlayerState] =
    val baseTroops = players.size match {
      case 2 => 40
      case 3 => 35
      case 4 => 30
      case 5 => 25
      case _ => 20
    }
    playerStates.map:
      playerState =>
        val playerId = playerState.playerId
        val territoriesOwned = board.territoriesOwnedBy(playerId).size
        val remainingTroops = baseTroops - territoriesOwned
        playerState.copy(bonusTroops = remainingTroops, phase = TurnPhase.SetupPhase)
