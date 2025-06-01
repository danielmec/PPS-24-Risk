package core.engine

import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers 
import model.board._
import model.cards._
import engine.DecksManager
import engine.DecksManagerImpl
import exceptions._

class DecksManagerTest extends AnyFunSuite with Matchers {

    // Setting territories with no neighbors, no owner and no troop
    val t1 = Territory("Northern Europe", Set(), None, 0)
    val t2 = Territory("Eastern Europe", Set(), None, 0)
    
    // Setting cards with territories
    val card1 = TerritoryCard(t1, CardImg.Infantry)  
    val card2 = TerritoryCard(t2, CardImg.Cavalry)   
    
    val emptyDecksManager = DecksManagerImpl(List(), List())
    val decksManager = DecksManagerImpl(
        territories = List(card1, card2),
        objectives = List())
    
    test("DecksManager should throw an exception if territories deck is empty") {
        assertThrows[NoTerritoriesCardsException] {
            emptyDecksManager.drawTerritory()
        }
    }

    test("DecksManager should throw an exception if objectives deck is empty") {
        assertThrows[NoObjectivesCardsException] {
            emptyDecksManager.drawObjective()
        }
    }
    
    test("DecksManager should return a card when drawing from non-empty deck") {
        val (newManager, drawnCard) = decksManager.drawTerritory()
        drawnCard should be (card1)
    }
}




