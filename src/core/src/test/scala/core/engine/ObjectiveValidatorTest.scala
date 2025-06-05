package core.engine

import org.scalatest.funsuite.AnyFunSuite
import model.cards.*
import model.player.*
import model.board.*
import engine.*

class ObjectiveValidatorTest extends AnyFunSuite :

  val player = PlayerImpl("1", "Alice", PlayerColor.Red, PlayerType.Human)
  val playerState = PlayerState(player, Set.empty, None)
  val otherPlayer = PlayerImpl("2", "Bob", PlayerColor.Blue, PlayerType.Human)
  val otherState = PlayerState(otherPlayer, Set.empty, None)

  val territory1 = Territory("T1", Set.empty, Some(player), 3)
  val territory2 = Territory("T2", Set.empty, Some(player), 2)
  val territory3 = Territory("T3", Set.empty, Some(otherPlayer), 1)
  val continent = Continent("Europe", Set(territory1, territory2), 5)
  val board = Board("game1", Set(continent.copy(territories = Set(territory1, territory2)), Continent("Asia", Set(territory3), 7)))

  val gameState = GameState(
    gameId = "game1",
    board = board,
    playerStates = List(playerState, otherState),
    turnManager = null,
    decksManager = null
  )

  test("ConquerTerritories: true se il giocatore possiede abbastanza territori con truppe sufficienti"):
    val obj = ObjectiveCard.ConquerTerritories(2, 2)
    assert(ObjectiveValidator.done(obj, gameState, playerState))

  test("ConquerContinents: true se il giocatore possiede tutti i territori del continente"):
    val obj = ObjectiveCard.ConquerContinents(Set(continent))
    assert(ObjectiveValidator.done(obj, gameState, playerState))

  test("DefeatPlayer: true se il giocatore target non possiede territori"):
    val obj = ObjectiveCard.DefeatPlayer(PlayerColor.Blue)
    val boardNoBlue = board.copy(continents = board.continents.map(c =>
      c.copy(territories = c.territories.map(t => t.copy(owner = Some(player))))
    ))
    val gameStateNoBlue = gameState.copy(board = boardNoBlue)
    assert(ObjectiveValidator.done(obj, gameStateNoBlue, playerState))

  test("ConquerNContinents: true se il giocatore possiede abbastanza continenti"):
    val obj = ObjectiveCard.ConquerNContinents(1)
    assert(ObjectiveValidator.done(obj, gameState, playerState))

  test("ConquerTerritories: false se non possiede abbastanza territori"):
    val obj = ObjectiveCard.ConquerTerritories(3, 2)
    assert(!ObjectiveValidator.done(obj, gameState, playerState))
