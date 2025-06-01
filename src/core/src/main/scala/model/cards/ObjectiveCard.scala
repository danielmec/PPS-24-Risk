package model.cards

import model.player.PlayerColor
import model.board.Continent

sealed trait ObjectiveCard extends Card

object ObjectiveCard:
  /** Conquista almeno `num` territori, ognuno con almeno `minTroops` truppe */
  case class ConquerTerritories(num: Int, minTroops: Int = 1) extends ObjectiveCard

  /** Conquista tutti i territori dei continenti specificati */
  case class ConquerContinents(continents: Set[Continent]) extends ObjectiveCard

  /** Elimina il giocatore di un certo colore */
  case class EliminatePlayer(targetColor: PlayerColor) extends ObjectiveCard

  /** Conquista almeno `n` continenti a scelta */
  case class ConquerNContinents(n: Int) extends ObjectiveCard
