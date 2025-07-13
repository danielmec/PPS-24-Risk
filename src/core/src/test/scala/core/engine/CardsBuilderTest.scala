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
    val russia = territories.find(_.name == "Russia").get
    val infantryCounter = territoriesDeck.count(_.cardImg == CardImg.Infantry)
    val cavalryCounter = territoriesDeck.count(_.cardImg == CardImg.Cavalry)
    val artilleryCounter = territoriesDeck.count(_.cardImg == CardImg.Artillery)

    test("CardsBuilder should get continents from JSON file, with correct parameters"):
        continents should have size 6
        cNames should contain allOf("North America", "South America", "Europe", "Africa", "Asia", "Australia")
        val europe = continents.find(_.name == "Europe").get
        europe.bonusTroops should be (5)

    test("CardsBuilder should get territories from JSON file, with correct parameters"):
        territories should have size 26 
        china.neighbors should contain (russia)
        // also the opposite should be true
        russia.neighbors should contain (china)
        // connections check        
        val mexico = territories.find(_.name == "Mexico").get
        val venezuela = territories.find(_.name == "Venezuela").get
        mexico.neighbors should contain (venezuela)
        venezuela.neighbors should contain (mexico)

    test("CardsBuilder should create a territories deck"):
        territoriesDeck should have size 26  
        tNames should have size 26
        infantryCounter + cavalryCounter + artilleryCounter should be (26)

    test("CardsBuilder should associate continents and territories properly"):
        northAmerica.territories should have size 6  
        northAmericaTerritories should contain allOf("Alaska", "Canada", "Greenland", "Western US", "Eastern US", "Mexico")
        val australia = continents.find(_.name == "Australia").get
        australia.territories should have size 4
        australia.territories.map(_.name).toSet should be(Set("Indonesia", "New Guinea", "Western Australia", "Eastern Australia"))

    test("CardsBuilder should create an objectives deck (objectives should be valid)"):
        objectivesDeck should not be empty
        val conquerContinents = objectivesDeck.collect: 
            case obj: ObjectiveCard.ConquerContinents => obj 
        conquerContinents should not be empty
        val conquerNContinents = objectivesDeck.collect:
            case obj: ObjectiveCard.ConquerNContinents => obj
        conquerNContinents should not be empty

    test("CardsBuilder should correctly set up key strategic connections"):
        val alaska = territories.find(_.name == "Alaska").get
        val russia = territories.find(_.name == "Russia").get
        alaska.neighbors should contain (russia)  
        val brazil = territories.find(_.name == "Brazil").get
        val northAfrica = territories.find(_.name == "North Africa").get
        brazil.neighbors should contain (northAfrica)  
        val indonesia = territories.find(_.name == "Indonesia").get
        val china = territories.find(_.name == "China").get
        indonesia.neighbors should contain (china) 