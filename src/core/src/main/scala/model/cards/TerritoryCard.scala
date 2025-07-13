package model.cards

import model.board.*

trait Card

enum CardImg:
    case Infantry
    case Cavalry
    case Artillery

case class TerritoryCard(
    territory: Territory,
    cardImg: CardImg
) extends Card