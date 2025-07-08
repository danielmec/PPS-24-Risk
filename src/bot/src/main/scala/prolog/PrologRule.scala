package prolog

import strategy.*
import engine.GameState
import engine.GameAction
import alice.tuprolog.*
import prolog.PrologEngine.given_Conversion_String_Theory
import prolog.PrologEngine.given_Conversion_String_Term
import exceptions._

/**
 * Trait che implementa una regola di strategia usando Prolog
 * @param theory nome del file Prolog (senza estensione)
 */
trait PrologRule(val theoryName: String) extends StrategyRule:

  private val engine: PrologEngine = PrologEngine("/theories/" + theoryName + ".pl")

  override def evaluateAction(gameState: GameState, playerId: String): Set[RatedAction] =
    val (territoriesStr, neighborStr) = encodeGameState(gameState)
    val phase = gameState.turnManager.currentPhase.toString
    val actionString = "Action"
    val scoreString = "Score" 
    val descString = "Description"
    val goal = s"${theoryName.toLowerCase}($territoriesStr, $neighborStr, '$phase', '$playerId', $actionString, $scoreString, $descString)"   
    val solutions = engine.solveAll(goal, actionString, scoreString, descString)
    val actions = solutions.map(terms => {
      val actionTerm = terms(actionString)
      val score = terms(scoreString).toString.toDouble
      val description = terms(descString).toString.replaceAll("'", "")        
      try {
        val parsedAction = parseAction(gameState, actionTerm, playerId)
        RatedAction(parsedAction, score, description)
      } catch {
        case e: Exception => throw e
      }
    }).toSet

    actions
  
  protected def encodeGameState(gameState: GameState): (String, String) = 
    val territoryMap = gameState.board.territories.map(t => t.name -> t).toMap
    val territoriesWithNeighbors = gameState.board.territories.map(t => 
      (t.name, t.neighbors.map(_.name).toList)
    )
    // territory('TerritoryName', 'OwnerId', Troops)
    val territoriesStr = gameState.board.territories.map { t =>
      val owner = t.owner.map(_.id).getOrElse("none")
      s"territory('${escapeName(t.name)}', '$owner', ${t.troops})"
    }.mkString("[", ",", "]")    
    // neighbor('NomeTerritorio', 'NomeVicino', 'IDProprietarioVicino'), Ex: neighbor('Cina', 'India', 'proprietarioIndia')
    val neighborStr = gameState.board.territories.flatMap { t =>
      t.neighbors.map { n => 
        val updatedNeighbor = territoryMap.getOrElse(n.name, n)
        val neighOwner = updatedNeighbor.owner.map(_.id).getOrElse("none")
        s"neighbor('${escapeName(t.name)}', '${escapeName(n.name)}', '$neighOwner')"
      }
    }.mkString("[", ",", "]")    
    (territoriesStr, neighborStr)
  
  private def escapeName(name: String): String = 
    name.replace("'", "\\'")
  

  protected def parseAction(gameState: GameState, actionTerm: Term, playerId: String): GameAction =
    
    if (actionTerm.toString.startsWith("place_troops"))   // functors
      val args = extractArgs(actionTerm)
      val territoryName = args(0)
      val troops = args(1).toInt
      GameAction.PlaceTroops(playerId, troops, territoryName)

    else if (actionTerm.toString.startsWith("reinforce"))
      val args = extractArgs(actionTerm)
      val from = args(0)
      val to = args(1)
      val troops = args(2).toInt
      GameAction.Reinforce(playerId, from, to, troops)

    else if (actionTerm.toString.startsWith("attack"))
      val args = extractArgs(actionTerm)
      val from = args(0)
      val to = args(1)
      val troops = args(2).toInt
      val defenderId = gameState.board.territories
        .find(_.name == to)
        .flatMap(_.owner)
        .map(_.id)
        .getOrElse(throw new InvalidActionException())
      GameAction.Attack(playerId, defenderId, from, to, troops)
     
    else if (actionTerm.toString.startsWith("end_turn")) then GameAction.EndTurn
    else throw new IllegalArgumentException(s"Unknown action: ${actionTerm.toString()}")
  
  private def extractArgs(term: Term): Array[String] = 
    val content = term.toString
    val argsStr = content.substring(content.indexOf("(") + 1, content.lastIndexOf(")"))
    argsStr.split(",").map(_.trim.replaceAll("'", ""))
