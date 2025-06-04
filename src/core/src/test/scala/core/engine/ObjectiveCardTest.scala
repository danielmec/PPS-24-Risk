package core.engine

import org.scalatest.funsuite.AnyFunSuite
import model.player.PlayerColor
import model.board.Continent

class ObjectiveCardTest extends AnyFunSuite:

  test("ConquerTerritories description is correct"): 
    val obj = ObjectiveCard.ConquerTerritories(5, 2)
    assert(obj.description == "Conquer 5 territories with at least 2 troops each.")

  test("ConquerContinents description is correct"):
    val continents = Set(Continent("Europe", Set.empty), Continent("Asia", Set.empty))
    val obj = ObjectiveCard.ConquerContinents(continents)
    assert(obj.description.contains("Europe"))
    assert(obj.description.contains("Asia"))

  test("DefeatPlayer description is correct"):
    val obj = ObjectiveCard.DefeatPlayer(PlayerColor.Red)
    assert(obj.description.contains("red armies"))

  test("ConquerNContinents description is correct"):
    val obj = ObjectiveCard.ConquerNContinents(3)
    assert(obj.description == "Conquer any 3 continents")
