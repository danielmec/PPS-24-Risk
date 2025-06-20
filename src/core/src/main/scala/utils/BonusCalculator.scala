package utils

import model.board._
import model.cards._
import model.player._
import engine._

object BonusCalculator:
  def calculateTradeBonus(cards: Set[TerritoryCard]): Int =
    val imgs = cards.map(_.cardImg)
    imgs.toList.sortBy(_.toString) match
      case List(CardImg.Artillery, CardImg.Artillery, CardImg.Artillery) => 4
      case List(CardImg.Infantry, CardImg.Infantry, CardImg.Infantry) => 6
      case List(CardImg.Cavalry, CardImg.Cavalry, CardImg.Cavalry) => 8
      case List(a, b, c) if Set(a, b, c).size == 3 && !imgs.contains(CardImg.Jolly) => 10
      case List(a, b, CardImg.Jolly) if a == b => 12
      case List(CardImg.Jolly, a, b) if a == b => 12
      case List(a, CardImg.Jolly, b) if a == b => 12
      case _ => 0

  def calculateStartTurnBonus(playerId: String, board: Board): Int =
      val territoriesOwned = board.territoriesOwnedBy(playerId).size
      val continentsOwned = board.continentsOwnedBy(playerId)
      val territoryBonus = math.max(3, territoriesOwned / 3)
      val continentBonus = continentsOwned.map(_.bonusTroops).sum  
      territoryBonus + continentBonus
  
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
        playerState.copy(bonusTroops = remainingTroops, phase = TurnPhase.PlacingTroops)
