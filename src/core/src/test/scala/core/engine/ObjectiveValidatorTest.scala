package core.engine

import org.scalatest.funsuite.AnyFunSuite
import model.cards.*
import model.player.*
import model.board.*
import engine.*
import engine.CardsBuilder

class ObjectiveValidatorTest extends AnyFunSuite :

  val player = PlayerImpl("1", "Alice", PlayerColor.Red, PlayerType.Human)
  val playerState = PlayerState(player, Set.empty, None)
  val otherPlayer = PlayerImpl("2", "Bob", PlayerColor.Blue, PlayerType.Human)
  val otherState = PlayerState(otherPlayer, Set.empty, None)

  val (continents, territoriesMap) = CardsBuilder.createBoard()
  val allTerritories = territoriesMap.values.toList
  val half = allTerritories.size / 2
  val player1Territories = allTerritories.take(half).map(_.copy(owner = Some(player)))
  val player2Territories = allTerritories.drop(half).map(_.copy(owner = Some(otherPlayer)))
  val updatedTerritories = (player1Territories ++ player2Territories).toSet

  val updatedContinents = continents.map: continent =>
    continent.copy(territories = continent.territories.map(t => updatedTerritories.find(_.name == t.name).get))

  val board = Board("game1", updatedContinents)
  val gameState = GameState(
    gameId = "game1",
    board = board,
    playerStates = List(playerState, otherState),
    turnManager = null,
    decksManager = null
  )

  test("ConquerTerritories: true se il giocatore possiede abbastanza territori con truppe sufficienti"):
    val obj = ObjectiveCard.ConquerTerritories(player1Territories.size, 1)
    assert(ObjectiveValidator.done(obj, gameState, playerState))

  test("ConquerContinents: true se il giocatore possiede tutti i territori di almeno un continente"):
    val ownedContinent = updatedContinents.find(c => c.territories.forall(_.owner.contains(player))).get
    val obj = ObjectiveCard.ConquerContinents(Set(ownedContinent))
    assert(ObjectiveValidator.done(obj, gameState, playerState))

  test("DefeatPlayer: true se il giocatore target non possiede territori"):
    val obj = ObjectiveCard.DefeatPlayer(PlayerColor.Blue)
    val boardNoBlue = board.copy(continents = board.continents.map(c =>
      c.copy(territories = c.territories.map(t => t.copy(owner = Some(player))))))
    val gameStateNoBlue = gameState.copy(board = boardNoBlue)
    assert(ObjectiveValidator.done(obj, gameStateNoBlue, playerState))

  test("ConquerNContinents: true se il giocatore possiede abbastanza continenti"):
    val obj = ObjectiveCard.ConquerNContinents(1)
    assert(ObjectiveValidator.done(obj, gameState, playerState))

  test("ConquerTerritories: false se non possiede abbastanza territori"):
    val obj = ObjectiveCard.ConquerTerritories(3, 2)
    assert(!ObjectiveValidator.done(obj, gameState, playerState))
