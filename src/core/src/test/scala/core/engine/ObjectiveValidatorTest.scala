package engine

import org.scalatest.funsuite.AnyFunSuite
import model.cards.*
import model.player.*
import model.board.*
import engine.*
import engine.CardsBuilder

class ObjectiveValidatorTest extends AnyFunSuite:
  val player = PlayerImpl("1", "Alice", PlayerColor.Red, PlayerType.Human)
  val otherPlayer = PlayerImpl("2", "Bob", PlayerColor.Blue, PlayerType.Human)
  val playerState = PlayerState(player, Set.empty, None, TurnPhase.MainPhase, 0)
  val otherState = PlayerState(otherPlayer, Set.empty, None, TurnPhase.MainPhase, 0)
  val (continents, territoriesMap) = CardsBuilder.createBoard()
  val allTerritories = territoriesMap.values.toList
  val half = allTerritories.size / 2
  val player1Territories = allTerritories.take(half).map(_.copy(owner = Some(player), troops = 1))
  val player2Territories = allTerritories.drop(half).map(_.copy(owner = Some(otherPlayer), troops = 1))
  val updatedTerritories = (player1Territories ++ player2Territories).toSet

  val updatedContinents = continents.map: continent =>
    continent.copy(territories = continent.territories.map(t => 
      updatedTerritories.find(_.name == t.name).get))

  val board = Board("game1", updatedContinents)
  val gameState = GameState(
    gameId = "game1",
    board = board,
    playerStates = List(playerState, otherState),
    turnManager = null,
    decksManager = null,
    objectiveCards = List.empty
  )

  test("ConquerContinents: returns true if player owns all territories of at least one continent"):
    val (gs, ownedContinent) = gameStateWithPlayerOwningAContinent
    val obj = ObjectiveCard.ConquerContinents(Set(ownedContinent))
    assert(ObjectiveValidator.done(Some(obj), gs, playerState))

  test("ConquerNContinents: returns true if player owns enough continents"):
    val (gs, _) = gameStateWithPlayerOwningAContinent
    val obj = ObjectiveCard.ConquerNContinents(1)
    assert(ObjectiveValidator.done(Some(obj), gs, playerState))

  test("ConquerTerritories: returns true if player owns enough territories with sufficient troops"):
    val obj = ObjectiveCard.ConquerTerritories(player1Territories.size, 1)
    assert(ObjectiveValidator.done(Some(obj), gameState, playerState))

  test("ConquerTerritories: returns false if player does not own enough territories"):
    val obj = ObjectiveCard.ConquerTerritories(3, 2)
    assert(!ObjectiveValidator.done(Some(obj), gameState, playerState))

  test("All territories must have unique names on the board"):
    assert(board.territories.groupBy(_.name).forall(_._2.size == 1))

  test("Returns false for None objective"):
    assert(!ObjectiveValidator.done(None, gameState, playerState))

  def gameStateWithPlayerOwningAContinent: (GameState, Continent) =
    val continentToOwn = updatedContinents.head
    val ownedTerritories = continentToOwn.territories.map(_.copy(owner = Some(player), troops = 1))
    val updatedContinent = continentToOwn.copy(territories = ownedTerritories)
    val updatedContinentsWithOneOwned = updatedContinents - continentToOwn + updatedContinent
    val boardWithOwnedContinent = Board("game1", updatedContinentsWithOneOwned)
    (gameState.copy(board = boardWithOwnedContinent), updatedContinent)