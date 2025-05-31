package model.cards
import model.board.*

case class TerritoryCard(
    territory: Territory,
    cardImg: CardImg
) extends Card:

    def checkTris(card2: TerritoryCard, card3: TerritoryCard): Boolean =
        // Set eliminates duplicates, so cardsImgs.size depends on the uniqueness of the imgs
        val cardsImgs = Set(this.cardImg, card2.cardImg, card3.cardImg)
        // All same imgs or all different imgs
        cardsImgs.size == 1 || cardsImgs.size == 3