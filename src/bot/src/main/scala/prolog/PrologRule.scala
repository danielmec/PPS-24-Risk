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

  /**
   * Genera azioni valutate basandosi sul file Prolog specificato
   * @param gameState stato corrente del gioco
   * @param playerId ID del giocatore per cui generare le azioni
   * @return insieme di azioni valutate generate dalla regola Prolog
   */
  override def evaluateAction(gameState: GameState, playerId: String): Set[RatedAction] =
    val encodedState = encodeGameState(gameState)
    val phase = gameState.turnManager.currentPhase.toString
    val actionString = "Action"
    val scoreString = "Score" 
    val descString = "Description"
    val goal = s"$theoryName($encodedState, '$phase', '$playerId', $actionString, $scoreString, $descString)"
    
    engine.solveAll(goal, actionString, scoreString, descString)
      .map(terms => {
        val actionTerm = terms(actionString)
        val score = terms(scoreString).toString.toDouble
        val description = terms(descString).toString.replaceAll("'", "")
        
        RatedAction(parseAction(gameState, actionTerm, playerId), score, description)
      }).toSet
      
  /**
   * Converte lo stato di gioco in un formato adatto per Prolog
   */
  protected def encodeGameState(gameState: GameState): String = 
    // Codifica di territori, giocatori e altre informazioni
    val territoriesStr = gameState.board.territories.map { t =>
      val owner = t.owner.map(_.id).getOrElse("none")
      s"territory('${t.name}', '$owner', ${t.troops})"
    }.mkString("[", ",", "]")

    // Codifica neighbor
    val neighborStr = gameState.board.territories.flatMap { t =>
      t.neighbors.map(n => s"neighbor('${t.name}', '${n.name}')")
    }.mkString("[", ",", "]")
    
    s"$territoriesStr, $neighborStr"
  
  
  /**
   * Converte un termine Prolog in un'azione di gioco
   */
  protected def parseAction(gameState: GameState, actionTerm: Term, playerId: String): GameAction =
    val functor = actionTerm.toString
    
    if (functor.startsWith("place_troops")) {
      val args = extractArgs(actionTerm)
      val territoryName = args(0)
      val troops = args(1).toInt
      GameAction.PlaceTroops(playerId, troops, territoryName)
    } 
    else if (functor.startsWith("attack")) {
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
    } 
    else if (functor.startsWith("end_phase")) {
      GameAction.EndPhase
    } 
    else if (functor.startsWith("end_turn")) {
      GameAction.EndTurn
    } 
    else {
      throw new IllegalArgumentException(s"Unknown action: $functor")
    }
  
  private def extractArgs(term: Term): Array[String] = 
    val content = term.toString
    val argsStr = content.substring(content.indexOf("(") + 1, content.lastIndexOf(")"))
    argsStr.split(",").map(_.trim.replaceAll("'", ""))
  