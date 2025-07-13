package engine

import model.board._
import model.cards._
import model.player._
import scala.io.Source
import scala.util.Failure
import scala.util.Try
import scala.util.Success
import scala.util.Random
import play.api.libs.json._
import exceptions._

/**
  * Utility object for building cards, continents, and objectives from JSON files.
  */
object CardsBuilder:
    private val boardPath = "src/core/src/main/scala/json/Board.json"
    private val objectivesPath = "src/core/src/main/scala/json/Objectives.json"

    /**
      * Loads a JSON file from the given path.
      * @param path The path to the JSON file.
      * @return The parsed JsValue.
      * @throws RuntimeException if the file cannot be loaded.
      */
    private def loadJsonFromFile(path: String): JsValue =
        Try {
            val jsonContent = Source.fromFile(path).getLines().mkString
            Json.parse(jsonContent)
        }.getOrElse {
            throw new RuntimeException(s"Failed to load JSON from $path")
        }

    /**
      * Returns all territories from the board.
      * @return A set of all territories.
      */
    def getTerritories(): Set[Territory] =
        val (_, territoriesMap) = createBoard()
        territoriesMap.values.toSet

    /**
      * Returns all continents from the board.
      * @return A set of all continents.
      */
    def getContinents(): Set[Continent] =
        val (continents, _) = createBoard()
        continents

    /**
      * Creates the board (continents and territories) from the JSON file.
      * @return A tuple of (Set of continents, Map of territory name to Territory).
      */
    def createBoard(): (Set[Continent], Map[String, Territory]) = 
        val json = loadJsonFromFile(boardPath)
        val continentsFromJson = (json \ "continents").as[JsArray].value.toSeq  
        val initialTerritories = createInitialTerritories(continentsFromJson) 
        val territoriesWithNeighbors = addNeighborsToTerritories(continentsFromJson, initialTerritories)
        val continents = createContinentsFromJson(continentsFromJson, territoriesWithNeighbors)
        (continents, territoriesWithNeighbors)

    /**
      * Creates initial territories from the JSON continents.
      * @param continentsJson The JSON array of continents.
      * @return A map of territory name to Territory.
      */
    private def createInitialTerritories(continentsJson: Seq[JsValue]): Map[String, Territory] =
    continentsJson.flatMap { continentJson =>
        (continentJson \ "territories").as[JsArray].value.map { territoryJson =>
            val name = (territoryJson \ "name").as[String]
            name -> Territory(name)
        }
    }.toMap

    /**
      * Adds neighbors to each territory based on the JSON data.
      * @param continentsJson The JSON array of continents.
      * @param territoriesMap The map of territory name to Territory.
      * @return The updated map of territory name to Territory with neighbors set.
      */
    private def addNeighborsToTerritories(
        continentsJson: Seq[JsValue],
        territoriesMap: Map[String, Territory]
    ): Map[String, Territory] =
        continentsJson.foldLeft(territoriesMap) { (accMap, continentJson) =>
            (continentJson \ "territories").as[JsArray].value.foldLeft(accMap) { (mapAcc, territoryJson) =>
                val name = (territoryJson \ "name").as[String]
                val neighborsNames = (territoryJson \ "neighbors").as[JsArray].value.map(_.as[String])
                val neighbors = neighborsNames.flatMap(n => mapAcc.get(n)).toSet
                val updatedTerritory = mapAcc(name).copy(neighbors = neighbors)
                mapAcc + (name -> updatedTerritory)
            }
        }

    /**
      * Creates continents from the JSON data.
      * @param continentsJson The JSON array of continents.
      * @param territoriesMap The map of territory name to Territory.
      * @return A set of Continent objects.
      */
    private def createContinentsFromJson(
        continentsJson: Seq[JsValue], 
        territoriesMap: Map[String, Territory]
    ): Set[Continent] =
        continentsJson.map { continentJson =>
            val name = (continentJson \ "name").as[String]
            val bonusTroops = (continentJson \ "bonusTroops").as[Int]
            val territoryNames = (continentJson \ "territories").as[JsArray].value.map(t => (t \ "name").as[String])
            val continentTerritories = territoryNames.flatMap(n => territoriesMap.get(n)).toSet
            Continent(name, continentTerritories, bonusTroops)
        }.toSet

    /**
      * Creates the deck of territory cards.
      * @return A list of TerritoryCard objects.
      */
    def createTerritoriesDeck(): List[TerritoryCard] =
        val (_, territoriesMap) = createBoard()
        val territories = territoriesMap.values.toList
        territories.zipWithIndex.map:
            case (territory, index) =>
                val cardImg = index % 3 match
                    case 0 => CardImg.Infantry
                    case 1 => CardImg.Cavalry
                    case 2 => CardImg.Artillery
                TerritoryCard(territory, cardImg)

    /**
      * Creates the deck of objective cards from the JSON file.
      * @return A list of ObjectiveCard objects.
      */
    def createObjectivesDeck(): List[ObjectiveCard] =
        val continents = getContinents()
        val continentsMap = continents.map(c => (c.name, c)).toMap
        val json = loadJsonFromFile(objectivesPath)       
        val continentObjectives = createContinentObjectives(json, continentsMap)
        val nContinentsObjectives = createNContinentsObjectives(json)
        val territoryObjectives = createTerritoryObjectives(json)
        continentObjectives ++ nContinentsObjectives ++ territoryObjectives

    /**
      * Creates continent objectives from the JSON data.
      * @param json The parsed objectives JSON.
      * @param continentsMap The map of continent name to Continent.
      * @return A list of ObjectiveCard.ConquerContinents.
      */
    private def createContinentObjectives(json: JsValue, continentsMap: Map[String, Continent]): List[ObjectiveCard] =
        (json \\ "continents").headOption.map { continentsJson =>
            continentsJson.as[JsArray].value.map { objJson =>
                val continentNames = (objJson \ "continents").as[JsArray].value.map(_.as[String])
                val continentsToConquer = continentNames.flatMap(name => continentsMap.get(name)).toSet
                ObjectiveCard.ConquerContinents(continentsToConquer)
            }.toList
        }.getOrElse(List.empty)

    /**
      * Creates N-continents objectives from the JSON data.
      * @param json The parsed objectives JSON.
      * @return A list of ObjectiveCard.ConquerNContinents.
      */
    private def createNContinentsObjectives(json: JsValue): List[ObjectiveCard] =
        (json \\ "nContinents").headOption.map { nContinentsJson =>
            nContinentsJson.as[JsArray].value.map { objJson =>
                val count = (objJson \ "count").as[Int]
                ObjectiveCard.ConquerNContinents(count)
            }.toList
        }.getOrElse(List.empty)

    /**
      * Creates territory objectives from the JSON data.
      * @param json The parsed objectives JSON.
      * @return A list of ObjectiveCard.ConquerTerritories.
      */
    private def createTerritoryObjectives(json: JsValue): List[ObjectiveCard] =
        (json \\ "territories").headOption.map { territoriesJson =>
            territoriesJson.as[JsArray].value.map { objJson =>
                val count = (objJson \ "count").as[Int]
                val minTroops = (objJson \ "minTroops").as[Int]
                ObjectiveCard.ConquerTerritories(count, minTroops)
            }.toList
        }.getOrElse(List.empty)
