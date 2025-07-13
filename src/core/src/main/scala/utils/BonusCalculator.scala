package utils

import model.board._
import model.cards._
import model.player._
import engine._

/**
 * Utility object per il calcolo dei bonus in base alle regole del gioco Risiko.
 */
object BonusCalculator:

  /**
   * Calcola il bonus truppe derivante dalla combinazione di tre carte territorio.
   *
   * @param cards Una sequenza di tre carte territorio giocate dal giocatore.
   * @return Il numero di truppe bonus ricevute in base al tris:
   *         - 4 per tre artiglierie
   *         - 6 per tre fanterie
   *         - 8 per tre cavallerie
   *         - 0 in tutti gli altri casi (inclusi tris non validi)
   */
  def calculateTradeBonus(cards: Seq[TerritoryCard]): Int =
    val imgs = cards.map(_.cardImg)
    println(s"[DEBUG] Calcolo tris per: $imgs")
    if (imgs.size != 3) 0
    else if (imgs.forall(_ == CardImg.Artillery)) 4
    else if (imgs.forall(_ == CardImg.Infantry)) 6
    else if (imgs.forall(_ == CardImg.Cavalry)) 8
    else 0

  /**
   * Calcola il bonus truppe a inizio turno per un giocatore.
   *
   * @param playerId L'identificativo del giocatore.
   * @param board Il board corrente della partita.
   * @return Il numero totale di truppe bonus, calcolato come:
   *         - Max(3, numero territori posseduti / 3)
   *         - + bonus derivanti dai continenti interamente posseduti
   */
  def calculateStartTurnBonus(playerId: String, board: Board): Int =
    val territoriesOwned = board.territoriesOwnedBy(playerId).size
    val continentsOwned = board.continentsOwnedBy(playerId)
    val territoryBonus = math.max(3, territoriesOwned / 3)
    val continentBonus = continentsOwned.map(_.bonusTroops).sum  
    territoryBonus + continentBonus

  /**
   * Calcola le truppe iniziali disponibili per ogni giocatore all'inizio della partita
   * e aggiorna lo stato dei giocatori.
   *
   * @param players La lista dei giocatori.
   * @param playerStates La lista degli stati iniziali dei giocatori.
   * @param board Il board corrente della partita.
   * @return Una nuova lista di stati dei giocatori, con truppe iniziali assegnate
   *         e la fase impostata a `SetupPhase`.
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
