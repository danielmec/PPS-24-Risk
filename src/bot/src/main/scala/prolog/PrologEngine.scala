package prolog

import strategy.*
import alice.tuprolog.*

trait PrologEngine:
  /**
   * Verifica se un goal è soddisfatto con le teorie registrate
   */
  def solveWithSuccess(goal: Term): Boolean

  /**
   * Risolve un goal e restituisce il valore del termine specificato
   */
  def solveOneAndGetTerm(goal: Term, term: String): Term

  /**
   * Risolve un goal e restituisce i valori di più termini specificati
   */
  def solveOneAndGetTerms(goal: Term, terms: String*): Map[String, Term]

  /**
   * Risolve un goal e restituisce tutte le soluzioni con i valori dei termini specificati
   */
  def solveAll(goal: Term, terms: String*): LazyList[Map[String, Term]]

  /**
   * Risolve un goal e restituisce tutte le soluzioni per un singolo termine
   */
  def solveAll(goal: Term, term: String): LazyList[Term]

object PrologEngine:
  // Conversioni implicite per facilitare l'uso
  given Conversion[String, Term] = Term.createTerm(_)
  given Conversion[Seq[_], Term] = _.mkString("[", ",", "]")
  // Conversione implicita da String (percorso file) a Theory
  given Conversion[String, Theory] = fileName =>
    val inputStream = getClass.getResourceAsStream(fileName)
    if (inputStream == null) {
      throw new IllegalArgumentException(s"Resource not found: $fileName")
    }
    try {
      new Theory(inputStream)
    } catch {
      case e: Exception => 
        throw new IllegalArgumentException(s"Error parsing Prolog file $fileName: ${e.getMessage}", e)
    }

  // Factory method per creare un PrologEngine da uno o più Theory
  def apply(theories: Theory*): PrologEngine = PrologEngineImpl(theories*)

  private case class PrologEngineImpl(theories: Theory*) extends PrologEngine:

    private val engine: Term => LazyList[SolveInfo] =
      val prolog = Prolog()
      theories.foreach(prolog.addTheory)
      goal =>
        new Iterable[SolveInfo] {
          override def iterator: Iterator[SolveInfo] = new Iterator[SolveInfo]:
            var solution: Option[SolveInfo] = Some(prolog.solve(goal))

            override def hasNext: Boolean = solution match
              case Some(value) => value.isSuccess || value.hasOpenAlternatives
              case None        => false

            override def next(): SolveInfo =
              val sol = solution match
                case Some(value) => value
                case None        => throw Exception()
              solution = if (sol.hasOpenAlternatives) Some(prolog.solveNext()) else None
              sol
        }.to(LazyList)

    override def solveWithSuccess(goal: Term): Boolean =
      engine(goal).map(_.isSuccess).headOption.contains(true)

    override def solveOneAndGetTerm(goal: Term, term: String): Term =
      engine(goal).headOption.map(extractTerm(_, term)) match
        case Some(value) => value
        case None        => "no."

    override def solveOneAndGetTerms(goal: Term, terms: String*): Map[String, Term] =
      engine(goal).headOption match
        case Some(value) => terms.map(term => (term, extractTerm(value, term))).toMap
        case None        => Map.empty

    override def solveAll(goal: Term, terms: String*): LazyList[Map[String, Term]] =
      engine(goal).map(solution =>
        (for (term <- terms)
          yield (term, extractTerm(solution, term))).toMap
      )

    override def solveAll(goal: Term, term: String): LazyList[Term] =
      engine(goal).map(extractTerm(_, term))

    private def extractTerm(solveInfo: SolveInfo, s: String): Term = solveInfo.getTerm(s)

