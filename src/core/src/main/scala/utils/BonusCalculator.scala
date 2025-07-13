package utils

import model.board._
import model.cards._
import model.player._
import engine._

object BonusCalculator:
  def calculateTradeBonus(cards: Seq[TerritoryCard]): Int =
    val imgs = cards.map(_.cardImg)
    println(s"[DEBUG] Calcolo tris per: $imgs")
    if (imgs.size != 3) 0
    else if (imgs.forall(_ == CardImg.Artillery)) 4
    else if (imgs.forall(_ == CardImg.Infantry)) 6
    else if (imgs.forall(_ == CardImg.Cavalry)) 8
    else 0

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
        playerState.copy(bonusTroops = remainingTroops, phase = TurnPhase.SetupPhase)
