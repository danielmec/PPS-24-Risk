package core.engine

import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers
import model.board._
import model.cards._
import engine.CardsBuilder

class CardsBuilderTest extends AnyFunSuite with Matchers:

    val continents = CardsBuilder.getContinents()
    val territories = CardsBuilder.getTerritories()
    val territoriesDeck = CardsBuilder.createTerritoriesDeck()
    val objectivesDeck = CardsBuilder.createObjectivesDeck()
    val cNames = continents.map(_.name)
    val tNames = territoriesDeck.map(_.territory.name).toSet
    val northAmerica = continents.find(_.name == "North America").get
    val northAmericaTerritories = northAmerica.territories.map(_.name)
    val china = territories.find(_.name == "China").get
    val japan = territories.find(_.name == "Japan").get
    val infantryCounter = territoriesDeck.count(_.cardImg == CardImg.Infantry)
    val cavalryCounter = territoriesDeck.count(_.cardImg == CardImg.Cavalry)
    val artilleryCounter = territoriesDeck.count(_.cardImg == CardImg.Artillery)

    test("CardsBuilder should get continents from JSON file, with correct parameters"):
        continents should have size 6
        cNames should contain allOf("North America", "South America", "Europe", "Africa", "Asia", "Australia")
        val europe = continents.find(_.name == "Europe").get
        europe.bonusTroops should be (4)

    test("CardsBuilder should get territories from JSON file, with correct parameters"):
        territories should have size 24
        china.neighbors should contain (japan)
        // also the opposite should be true
        japan.neighbors should contain (china)

    test("CardsBuilder should create a territories deck"):
        territoriesDeck should have size 24
        tNames should have size 24
        infantryCounter should be (8)
        cavalryCounter should be (8)
        artilleryCounter should be (8)

    test("CardsBuilder should associate continents and territories properly"):
        northAmerica.territories should have size 4
        northAmericaTerritories should contain allOf("Alaska", "Western US", "Eastern US", "Central America")

    test("CardsBuilder should create an objectives deck (objectives should be valid)"):
        objectivesDeck should not be empty
        val conquerContinents = objectivesDeck.collect: 
            case obj: ObjectiveCard.ConquerContinents => obj 
        conquerContinents should not be empty

        val conquerNContinents = objectivesDeck.collect:
            case obj: ObjectiveCard.ConquerNContinents => obj
        conquerNContinents should not be empty

        val conquerTerritories = objectivesDeck.collect:
            case obj: ObjectiveCard.ConquerTerritories => obj
        conquerTerritories should not be empty

        val defeatPlayers = objectivesDeck.collect:
            case obj: ObjectiveCard.DefeatPlayer => obj
        defeatPlayers should not be empty