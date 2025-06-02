package engine
import model.board._
import model.cards._
import scala.io.Source
import scala.util.{Try, Success, Failure}
import scala.util.Random
import scala.io.Source
import play.api.libs.json._  

object CardsBuilder:
    
    private val path = "src/core/src/main/scala/json/Board.json"
  
    private def loadData(): JsValue = 
        val jsonContent = Source.fromFile(path).mkString
        Json.parse(jsonContent)

    def createBoard(): (Set[Continent], Map[String, Territory]) = 
        val json = loadData()
        val continentsFromJson = (json \ "continents").as[JsArray].value 
        // Empty territories map
        var territoriesMap = Map.empty[String, Territory]
        continentsFromJson.foreach:
            continentJson =>   val territoriesJson = (continentJson \ "territories").as[JsArray].value
                                territoriesJson.foreach:
                                    territoryJson =>
                                    val name = (territoryJson \ "name").as[String]
                                    val territory = Territory(name, Set(), None, 0)
                                    territoriesMap += (name -> territory)
                            
        // Filling up neighbors for each territory
        continentsFromJson.foreach: 
            continentJson =>
                val territoriesJson = (continentJson \ "territories").as[JsArray].value
                territoriesJson.foreach:
                    territoryJson =>
                        val name = (territoryJson \ "name").as[String]
                        val neighborsNames = (territoryJson \ "neighbors").as[JsArray].value.map(_.as[String])
                        val neighbors = neighborsNames.flatMap(n => territoriesMap.get(n)).toSet
                        val updatedTerritory = territoriesMap(name).copy(neighbors = neighbors)
                        territoriesMap += (name -> updatedTerritory)
        
        // Continents map
        val continents = continentsFromJson.map:
            continentJson =>
                val name = (continentJson \ "name").as[String]
                val bonusTroops = (continentJson \ "bonusTroops").as[Int]
                val territoryNames = (continentJson \ "territories").as[JsArray].value.map(t => (t \ "name").as[String])
                val continentTerritories = territoryNames.flatMap(n => territoriesMap.get(n)).toSet
                Continent(name, continentTerritories, bonusTroops)
        .toSet

        (continents, territoriesMap)
  
    def createTerritoriesDeck(): List[TerritoryCard] =
        val (_, territoriesMap) = createBoard()
        val territories = territoriesMap.values.toList
        val cards = territories.zipWithIndex.map:
            case (territory, index) =>
                val cardImg = index % 3 match
                    case 0 => CardImg.Infantry
                    case 1 => CardImg.Cavalry
                    case 2 => CardImg.Artillery
                TerritoryCard(territory, cardImg)
        cards
        
    def getTerritories(): Set[Territory] =
        val (_, territoriesMap) = createBoard()
        territoriesMap.values.toSet
    
    def getContinents(): Set[Continent] =
        val (continents, _) = createBoard()
        continents