package utils

import model.board._
import model.cards._
import model.player._

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
