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
    PlayerState(p, Set.empty, None, TurnPhase.WaitingForTurn, 0)

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

        case GameAction.Reinforce(playerId, from, to, troops) =>
          val maybeFrom = gameState.board.territories.find(_.name == from)
          val maybeTo = gameState.board.territories.find(_.name == to)
          (maybeFrom, maybeTo) match
            case (Some(fromTerritory), Some(toTerritory))
              if fromTerritory.owner.exists(_.id == playerId) &&
                 toTerritory.owner.exists(_.id == playerId) &&
                 fromTerritory.troops > troops && troops > 0 &&
                 fromTerritory.neighbors.exists(_.name == toTerritory.name) =>
              val updatedFrom = fromTerritory.copy(troops = fromTerritory.troops - troops)
              val updatedTo = toTerritory.copy(troops = toTerritory.troops + troops)
              val updatedBoard = gameState.board
                .updatedTerritory(updatedFrom)
                .updatedTerritory(updatedTo)
              gameState = gameState.updateBoard(updatedBoard)
              Right(gameState)
            case _ =>
              Left("Territori non validi, non posseduti, non adiacenti o numero di truppe non valido.")

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
          val currentPlayerId = gameState.turnManager.currentPlayer.id
          val maybePlayerState = gameState.playerStates.find(_.playerId == currentPlayerId)
          maybePlayerState match
            case Some(playerState) =>
              if (cards.size != 3)
                Left("Devi scambiare esattamente 3 carte territorio.")
              else if (!cards.subsetOf(playerState.territoryCards))
                Left("Non possiedi tutte le carte territorio che vuoi scambiare.")
              else
                val bonus = calculateTradeBonus(cards)
                if (bonus == 0)
                  Left("Combinazione di carte non valida per il bonus.")
                else
                  val updatedPlayerState = playerState
                    .removeTerritoryCards(cards)
                    .copy(bonusTroops = playerState.bonusTroops + bonus)
                  val updatedPlayerStates = gameState.playerStates.map {
                    case ps if ps.playerId == currentPlayerId => updatedPlayerState
                    case ps => ps
                  }
                  gameState = gameState.copy(playerStates = updatedPlayerStates)
                  Right(gameState)
            case None =>
              Left("Giocatore non trovato.")

        case GameAction.EndAttack | GameAction.EndPhase | GameAction.EndTurn =>
          turnManager = turnManager.nextPhase()
          gameState = gameState.updateTurnManager(turnManager)
          Right(gameState)

  def getGameState: GameState = gameState

  def checkVictory: Option[PlayerState] =
    gameState.checkWinCondition

  private def calculateTradeBonus(cards: Set[TerritoryCard]): Int =
    val imgs = cards.map(_.cardImg)
    imgs.toList.sortBy(_.toString) match
      case List(CardImg.Artillery, CardImg.Artillery, CardImg.Artillery) => 4
      case List(CardImg.Infantry, CardImg.Infantry, CardImg.Infantry) => 6
      case List(CardImg.Cavalry, CardImg.Cavalry, CardImg.Cavalry) => 8
      case List(a, b, c) if Set(a, b, c).size == 3 && !imgs.contains(CardImg.Jolly) => 10
      case List(a, b, CardImg.Jolly) if a == b => 12
      case List(CardImg.Jolly, a, b) if a == b => 12
      case List(a, CardImg.Jolly, b) if a == b => 12
      case _ => 0

