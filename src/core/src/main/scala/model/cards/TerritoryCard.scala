package model.cards

import model.board.*

/**
  * Marker trait for all card types.
  */
trait Card

/**
  * Enumeration of possible images/types for territory cards.
  */
enum CardImg:
    case Infantry
    case Cavalry
    case Artillery

/**
  * Represents a territory card, associated with a territory and a card image/type.
  * @param territory The territory associated with the card.
  * @param cardImg The image/type of the card.
  */
case class TerritoryCard(
    territory: Territory,
    cardImg: CardImg
) extends Card