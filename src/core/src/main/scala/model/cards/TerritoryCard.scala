package model.cards

import model.board.*

case class TerritoryCard(
    territory: Territory,
    cardImg: CardImg
) extends Card:

    def checkTris(card2: TerritoryCard, card3: TerritoryCard): Boolean =
        
        val cardsImgs = Set(this.cardImg, card2.cardImg, card3.cardImg)
        
        cardsImgs.size == 1 || cardsImgs.size == 3