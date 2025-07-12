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

object CardsBuilder:
    private val boardPath = "src/core/src/main/scala/json/Board.json"
    private val objectivesPath = "src/core/src/main/scala/json/Objectives.json"

    private def loadJsonFromFile(path: String): JsValue =
        Try {
            val jsonContent = Source.fromFile(path).getLines().mkString
            Json.parse(jsonContent)
        }.getOrElse {
            throw new RuntimeException(s"Failed to load JSON from $path")
        }

    def getTerritories(): Set[Territory] =
        val (_, territoriesMap) = createBoard()
        territoriesMap.values.toSet
    
    def getContinents(): Set[Continent] =
        val (continents, _) = createBoard()
        continents

    def createBoard(): (Set[Continent], Map[String, Territory]) = 
        val json = loadJsonFromFile(boardPath)
        val continentsFromJson = (json \ "continents").as[JsArray].value.toSeq  
        var territoriesMap = createInitialTerritories(continentsFromJson) 
        territoriesMap = addNeighborsToTerritories(continentsFromJson, territoriesMap)
        val continents = createContinentsFromJson(continentsFromJson, territoriesMap)
        (continents, territoriesMap)
    
    private def createInitialTerritories(continentsJson: Seq[JsValue]): Map[String, Territory] =
      continentsJson.flatMap { continentJson =>
        val territoriesJson = (continentJson \ "territories").as[JsArray].value
        territoriesJson.map { territoryJson =>
          val name = (territoryJson \ "name").as[String]
          name -> Territory(name)
        }
      }.toMap
    
    private def addNeighborsToTerritories(
        continentsJson: Seq[JsValue],
        territoriesMap: Map[String, Territory]
    ): Map[String, Territory] =
      continentsJson.flatMap { continentJson =>
        val territoriesJson = (continentJson \ "territories").as[JsArray].value
        territoriesJson.map { territoryJson =>
          val name = (territoryJson \ "name").as[String]
          val neighborsNames = (territoryJson \ "neighbors").as[JsArray].value.map(_.as[String])
          val neighbors = neighborsNames.flatMap(territoriesMap.get).toSet
          name -> territoriesMap(name).copy(neighbors = neighbors)
        }
      }.toMap
    
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

    def createObjectivesDeck(): List[ObjectiveCard] =
        val continents = getContinents()
        val continentsMap = continents.map(c => (c.name, c)).toMap
        val json = loadJsonFromFile(objectivesPath)       
        val continentObjectives = createContinentObjectives(json, continentsMap)
        val nContinentsObjectives = createNContinentsObjectives(json)
        val territoryObjectives = createTerritoryObjectives(json)
        continentObjectives ++ nContinentsObjectives ++ territoryObjectives

    private def createContinentObjectives(json: JsValue, continentsMap: Map[String, Continent]): List[ObjectiveCard] =
        (json \\ "continents").headOption.map { continentsJson =>
            continentsJson.as[JsArray].value.map { objJson =>
                val continentNames = (objJson \ "continents").as[JsArray].value.map(_.as[String])
                val continentsToConquer = continentNames.flatMap(name => continentsMap.get(name)).toSet
                ObjectiveCard.ConquerContinents(continentsToConquer)
            }.toList
        }.getOrElse(List.empty)
    
    private def createNContinentsObjectives(json: JsValue): List[ObjectiveCard] =
        (json \\ "nContinents").headOption.map { nContinentsJson =>
            nContinentsJson.as[JsArray].value.map { objJson =>
                val count = (objJson \ "count").as[Int]
                ObjectiveCard.ConquerNContinents(count)
            }.toList
        }.getOrElse(List.empty)
    
    private def createTerritoryObjectives(json: JsValue): List[ObjectiveCard] =
        (json \\ "territories").headOption.map { territoriesJson =>
            territoriesJson.as[JsArray].value.map { objJson =>
                val count = (objJson \ "count").as[Int]
                val minTroops = (objJson \ "minTroops").as[Int]
                ObjectiveCard.ConquerTerritories(count, minTroops)
            }.toList
        }.getOrElse(List.empty)
