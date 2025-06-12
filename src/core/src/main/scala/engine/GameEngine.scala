package engine

import model.player.*
import model.cards.*
import model.board.*
import exceptions._
import utils.*
import utils.GameEngineUtils.* // AGGIUNTO: Import dei metodi di supporto

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

  // CORRETTO: Rimossi campi duplicati dal GameState
  private var gameState: GameState = GameState(
    gameId = gameId,
    board = board,
    playerStates = playerStates,
    turnManager = turnManager,
    decksManager = decksManager
  )

  private var pendingAttack: Option[(PlayerImpl, PlayerImpl, Territory, Territory, Int)] = None
  private var territoryConqueredThisTurn: Boolean = false
  private var strategicMoveUsed: Boolean = false // AGGIUNTO: Flag per movimento strategico

  /** Esegue un'azione di gioco e aggiorna lo stato */
  def performAction(action: GameAction): Either[String, GameState] = 
    if (!gameState.turnManager.isValidAction(action))
      Left("Azione non valida per la fase o il giocatore corrente.")
    else 
      action match
        case GameAction.PlaceTroops(playerId, troops, territoryName) =>
          val maybeTerritory = gameState.board.territories.find(_.name == territoryName)
          val maybePlayerState = gameState.playerStates.find(_.playerId == playerId)
          (maybeTerritory, maybePlayerState) match
            case (Some(territory), Some(playerState)) if territory.owner.exists(_.id == playerId) =>
              if (troops <= 0 || troops > playerState.bonusTroops)
                Left(s"Numero di truppe da piazzare non valido. Puoi piazzare al massimo ${playerState.bonusTroops} truppe.")
              else
                val updatedTerritory = territory.copy(troops = territory.troops + troops)
                val updatedBoard = gameState.board.updatedTerritory(updatedTerritory)
                val updatedPlayerState = playerState.copy(bonusTroops = playerState.bonusTroops - troops)
                val updatedPlayerStates = gameState.playerStates.map:
                  case ps if ps.playerId == playerId => updatedPlayerState
                  case ps => ps
                gameState = gameState
                  .updateBoard(updatedBoard)
                  .copy(playerStates = updatedPlayerStates)
                Right(gameState)
            case _ =>
              Left("Territorio non valido o non posseduto dal giocatore.")

        case GameAction.Reinforce(playerId, from, to, troops) =>
          if (gameState.turnManager.currentPhase == TurnPhase.Reinforcement && strategicMoveUsed) {
            Left("Movimento strategico già utilizzato in questo turno.")
          } else {
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
                
                if (gameState.turnManager.currentPhase == TurnPhase.Reinforcement) {
                  strategicMoveUsed = true
                }
                
                Right(gameState)
              case _ =>
                Left("Territori non validi, non posseduti, non adiacenti o numero di truppe non valido.")
          }

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
                pendingAttack = Some((attacker, defender, attackerTerritory, defenderTerritory, troops))
                Right(gameState)
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

                  if (updatedDefenderTerritory.owner.exists(_.id == attacker.id))
                    territoryConqueredThisTurn = true
                  
                  val updatedBoard = gameState.board
                    .updatedTerritory(updatedAttackerTerritory)
                    .updatedTerritory(updatedDefenderTerritory)
                  gameState = gameState.updateBoard(updatedBoard)
                  
                  if (!hasRemainingTerritories(gameState, defender.id)) {
                    gameState = transferCardsOnElimination(gameState, defender.id, attacker.id)
                  }
                  
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
                val bonus = BonusCalculator.calculateTradeBonus(cards)
                if (bonus == 0)
                  Left("Combinazione di carte non valida per il bonus.")
                else
                  val updatedPlayerState = playerState
                    .removeTerritoryCards(cards)
                    .copy(bonusTroops = playerState.bonusTroops + bonus)
                  val updatedPlayerStates = gameState.playerStates.map:
                    case ps if ps.playerId == currentPlayerId => updatedPlayerState
                    case ps => ps
                  gameState = gameState.copy(playerStates = updatedPlayerStates)
                  Right(gameState)
            case None =>
              Left("Giocatore non trovato.")

        case GameAction.EndAttack | GameAction.EndPhase | GameAction.EndTurn =>
          if (action == GameAction.EndTurn) 
            strategicMoveUsed = false
            
            if (territoryConqueredThisTurn)
              val currentPlayerId = turnManager.currentPlayer.id
              val (updatedGameState, updatedDecksManager) = drawTerritoryCard(gameState, decksManager, currentPlayerId)
              gameState = updatedGameState
              decksManager = updatedDecksManager
              territoryConqueredThisTurn = false
          
          turnManager = turnManager.nextPhase()
          
          if turnManager.currentPhase == TurnPhase.PlacingTroops then
            val currentPlayerId = turnManager.currentPlayer.id
            val bonus = BonusCalculator.calculateStartTurnBonus(currentPlayerId, gameState.board)
            val updatedPlayerStates = gameState.playerStates.map:
              case ps if ps.playerId == currentPlayerId =>
                ps.copy(bonusTroops = ps.bonusTroops + bonus)
              case ps => ps
            gameState = gameState.copy(playerStates = updatedPlayerStates)
          
          gameState = gameState.updateTurnManager(turnManager)
  
          checkVictory match
            case Some(winner) => 
              Left(s"Partita terminata! Il vincitore è ${winner.player.name}!")
            case None => 
              Right(gameState)

  def setGameState(newState: GameState): Unit =
    gameState = newState

  def getGameState: GameState = gameState

  def checkVictory: Option[PlayerState] =
    gameState.checkWinCondition

