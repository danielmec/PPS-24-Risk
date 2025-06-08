package engine

import model.player.*
import model.cards.*
import model.board.*
import exceptions._

class GameEngine(
    val players: List[PlayerImpl],
    val gameId: String = java.util.UUID.randomUUID().toString
):
  private val (continents, territoriesMap) = CardsBuilder.createBoard()
  private val board = Board(gameId, continents)
  private val territoryDeck = CardsBuilder.createTerritoriesDeck()
  private val objectiveDeck = CardsBuilder.createObjectivesDeck()

  private val playerStates: List[PlayerState] = players.map: p =>
    PlayerState(p, Set.empty, None)

  private var turnManager: TurnManager = TurnManagerImpl(players)
  private var decksManager: DecksManager = DecksManagerImpl(territoryDeck, objectiveDeck)

  private var gameState: GameState = GameState(
    gameId = gameId,
    board = board,
    playerStates = playerStates,
    turnManager = turnManager,
    decksManager = decksManager,
    territoryCards = territoryDeck,
    objectiveCards = objectiveDeck
  )

  private var pendingAttack: Option[(PlayerImpl, PlayerImpl, Territory, Territory, Int)] = None

  /** Esegue un'azione di gioco e aggiorna lo stato */
  def performAction(action: GameAction): Either[String, GameState] = 
    if (!gameState.turnManager.isValidAction(action))
      Left("Azione non valida per la fase o il giocatore corrente.")
    else 
      action match
        case GameAction.PlaceTroops(playerId, troops, territoryName) =>
          val maybeTerritory = gameState.board.territories.find(_.name == territoryName)
          maybeTerritory match
            case Some(territory) if territory.owner.exists(_.id == playerId) =>
              val updatedTerritory = territory.copy(troops = territory.troops + troops)
              val updatedBoard = gameState.board.updatedTerritory(updatedTerritory)
              gameState = gameState.updateBoard(updatedBoard)
              Right(gameState)
            case _ =>
              Left("Territorio non valido o non posseduto dal giocatore.")

        case GameAction.Reinforce(playerId, troops) =>
          Right(gameState) // TODO

        case GameAction.Attack(attackerId, defenderId, from, to, troops) =>
          val maybeAttacker = players.find(_.id == attackerId)
          val maybeDefender = players.find(_.id == defenderId)
          val maybeAttackerTerritory = gameState.board.territories.find(_.name == from)
          val maybeDefenderTerritory = gameState.board.territories.find(_.name == to)

          (maybeAttacker, maybeDefender, maybeAttackerTerritory, maybeDefenderTerritory) match
            case (Some(attacker), Some(defender), Some(attackerTerritory), Some(defenderTerritory)) =>
              if (troops <= 0 || troops >= attackerTerritory.troops)
                Left("Numero di truppe non valido per l'attacco.")
              else if (!attackerTerritory.owner.contains(attacker) || !defenderTerritory.owner.contains(defender))
                Left("I territori non sono posseduti dai giocatori corretti.")
              else 
                //Attacco in sospeso
                pendingAttack = Some((attacker, defender, attackerTerritory, defenderTerritory, troops))
                Right(gameState) //Attende la difesa
            case _ =>
              Left("Territori o giocatori non validi per l'attacco.")

        case GameAction.Defend(defenderId, territoryName, defendTroops) =>
          pendingAttack match
            case Some((attacker, defender, attackerTerritory, defenderTerritory, attackingTroops)) 
              if defender.id == defenderId && defenderTerritory.name == territoryName =>
                val maxDefend = math.min(3, defenderTerritory.troops)
                if (defendTroops <= 0 || defendTroops > maxDefend)
                  Left(s"Numero di truppe di difesa non valido (max: $maxDefend).")
                else
                  val defenderDiceRoll: Int => Seq[Int] = _ => utils.Dice.roll(defendTroops)
                  val (result, updatedAttackerTerritory, updatedDefenderTerritory) =
                    Battle.battle(
                      attacker,
                      defender,
                      attackerTerritory,
                      defenderTerritory,
                      attackingTroops,
                      attackerDiceRoll = utils.Dice.roll,
                      defenderDiceRoll = defenderDiceRoll
                    )
                  val updatedBoard = gameState.board
                    .updatedTerritory(updatedAttackerTerritory)
                    .updatedTerritory(updatedDefenderTerritory)
                  gameState = gameState.updateBoard(updatedBoard)
                  pendingAttack = None
                  Right(gameState)
            case _ =>
              Left("Nessun attacco in sospeso o dati difensore non validi.")

        case GameAction.TradeCards(cards) =>
          Right(gameState) // TODO

        case GameAction.EndAttack | GameAction.EndPhase | GameAction.EndTurn =>
          turnManager = turnManager.nextPhase()
          gameState = gameState.updateTurnManager(turnManager)
          Right(gameState)

  def getGameState: GameState = gameState

  def checkVictory: Option[PlayerState] =
    gameState.checkWinCondition

