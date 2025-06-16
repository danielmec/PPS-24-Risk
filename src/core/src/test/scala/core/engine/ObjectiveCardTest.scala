package core.engine

import org.scalatest.funsuite.AnyFunSuite
import model.player.PlayerColor
import model.board.Continent
import model.cards.ObjectiveCard
import engine.CardsBuilder

class ObjectiveCardTest extends AnyFunSuite:

  val (continents, _) = CardsBuilder.createBoard()

  test("ConquerTerritories description is correct"): 
    val obj = ObjectiveCard.ConquerTerritories(5, 2)
    assert(obj.description == "Conquer 5 territories with at least 2 troops each.")

  test("ConquerContinents description is correct from JSON"):
    val someContinents = continents.take(2)
    val obj = ObjectiveCard.ConquerContinents(someContinents)
    someContinents.foreach: c =>
      assert(obj.description.contains(c.name))

  test("DefeatPlayer description is correct"):
    val obj = ObjectiveCard.DefeatPlayer(PlayerColor.Red)
    assert(obj.description.contains("red armies"))

  test("ConquerNContinents description is correct"):
    val obj = ObjectiveCard.ConquerNContinents(3)
    assert(obj.description == "Conquer any 3 continents")
