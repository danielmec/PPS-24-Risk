package engine

import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers 
import model.board._
import model.cards._
import engine.DecksManager
import engine.DecksManagerImpl
import exceptions._
import model.player.PlayerColor

class DecksManagerTest extends AnyFunSuite with Matchers:

    // Setting territories with no neighbors, no owner and no troop
    val t1 = Territory("Northern Europe", None, 0, Set())
    val t2 = Territory("Eastern Europe", None, 0, Set())
    val c1 = Continent("Europe", Set(t1, t2), 5)
    
    // Setting cards with territories and objectives
    val tCard1 = TerritoryCard(t1, CardImg.Infantry)  
    val tCard2 = TerritoryCard(t2, CardImg.Cavalry)
    val oCard1 = ObjectiveCard.ConquerContinents(Set(c1))
    val oCard2 = ObjectiveCard.ConquerNContinents(1)
    val oCard3 = ObjectiveCard.ConquerTerritories(3, 2)
    val oCard4 = ObjectiveCard.ConquerTerritories(12, 1)
    
    val emptyDecksManager = DecksManagerImpl(List(), List())

    val tDecksManager = DecksManagerImpl(
        territories = List(tCard1, tCard2), // deck with 2 territories
        objectives = List())

    val oDecksManager = DecksManagerImpl(
        territories = List(),
        objectives = List(oCard1, oCard2, oCard3, oCard4) // deck with 4 objectives
    )
    
    test("DecksManager should throw an exception if territories deck is empty"):
        assertThrows[NoTerritoriesCardsException]:
            emptyDecksManager.drawTerritory()
        
    test("DecksManager should throw an exception if objectives deck is empty"):
        assertThrows[NoObjectivesCardsException]:
            emptyDecksManager.drawObjective()
        
    test("DecksManager should return a card when drawing from non-empty deck"):
        val (newManager1, tDrawnCard) = tDecksManager.drawTerritory()
        val (newManager2, oDrawnCard) = oDecksManager.drawObjective()
        tDrawnCard should be (tCard1)
        oDrawnCard should be (oCard1)

    test("DecksManager should update decks when drawing cards"):
        val (newManager1, _) = tDecksManager.drawTerritory()
        val (newManager2, _) = oDecksManager.drawObjective()
        newManager1.territoriesCards should be (List(tCard2))
        newManager1.territoriesCards.size should be (1)
        newManager2.objectiveCards should be (List(oCard2, oCard3, oCard4))
        newManager2.objectiveCards.size should be (3)

    test("DecksManager should keep the same amount of cards after shuffling"):
        val newManager1 = tDecksManager.shuffleTerritoriesDeck()
        val newManager2 = oDecksManager.shuffleObjectivesDeck()
        newManager1.territoriesCards.size should be (2)
        newManager2.objectiveCards.size should be (4)