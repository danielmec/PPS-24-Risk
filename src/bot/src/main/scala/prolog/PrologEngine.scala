package prolog

import strategy.*
import alice.tuprolog.*

/**
 * Trait for a Prolog engine that can solve goals using registered theories.
 */
trait PrologEngine:
  /**
   * Checks if a goal is satisfied with the registered theories.
   * @param goal The Prolog goal to solve.
   * @return True if the goal is satisfied, false otherwise.
   */
  def solveWithSuccess(goal: Term): Boolean

  /**
   * Solves a goal and returns the value of the specified term.
   * @param goal The Prolog goal to solve.
   * @param term The name of the term to extract.
   * @return The value of the term as a Prolog Term.
   */
  def solveOneAndGetTerm(goal: Term, term: String): Term

  /**
   * Solves a goal and returns the values of multiple specified terms.
   * @param goal The Prolog goal to solve.
   * @param terms The names of the terms to extract.
   * @return A map from term names to their values.
   */
  def solveOneAndGetTerms(goal: Term, terms: String*): Map[String, Term]

  /**
   * Solves a goal and returns all solutions with the values of the specified terms.
   * @param goal The Prolog goal to solve.
   * @param terms The names of the terms to extract.
   * @return A LazyList of maps from term names to their values.
   */
  def solveAll(goal: Term, terms: String*): LazyList[Map[String, Term]]

  /**
   * Solves a goal and returns all solutions for a single term.
   * @param goal The Prolog goal to solve.
   * @param term The name of the term to extract.
   * @return A LazyList of Prolog Terms.
   */
  def solveAll(goal: Term, term: String): LazyList[Term]

/**
 * Companion object for PrologEngine.
 * Provides implicit conversions and a factory method for creating PrologEngine instances.
 */
object PrologEngine:
  given Conversion[String, Term] = Term.createTerm(_)
  given Conversion[Seq[_], Term] = _.mkString("[", ",", "]")
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

