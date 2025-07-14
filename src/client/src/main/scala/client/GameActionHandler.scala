package client

import scala.concurrent.Future
import scala.concurrent.ExecutionContext 
import client.ClientJsonSupport.GameActionMessage

/**
 * Handles game action requests from the client to the server in the Risk game.
 *
 * This class provides methods for all possible game actions a player can take,
 * formatting them as appropriate GameActionMessage objects and sending them
 * through the provided NetworkManager.
 *
 * @param networkManager The client network manager used to send messages to the server
 * @param ec The execution context for handling futures
 */
class GameActionHandler(networkManager: ClientNetworkManager)(implicit ec: ExecutionContext) {

  /**
   * Sends a request to place troops on a territory.
   *
   * @param gameId The ID of the current game
   * @param territory The name of the territory to place troops on
   * @param troops The number of troops to place
   * @return Future indicating whether the message was successfully sent
   */
  def placeTroops(gameId: String, territory: String, troops: Int): Future[Boolean] = {
    val action = GameActionMessage(
      gameId = gameId,
      action = "place_troops",
      parameters = Map(
        "territory" -> territory,
        "troops" -> troops.toString
      )
    )
    networkManager.sendMessage(action)
  }

  /**
   * Sends a request to attack a territory from another territory.
   *
   * @param gameId The ID of the current game
   * @param fromTerritory The name of the territory to attack from
   * @param toTerritory The name of the territory to attack
   * @param troops The number of troops to use in the attack
   * @param defenderId The ID of the defending player
   * @return Future indicating whether the message was successfully sent
   */
  def attack(
    gameId: String, 
    fromTerritory: String, 
    toTerritory: String, 
    troops: Int, 
    defenderId: String
  ): Future[Boolean] = {
    val action = GameActionMessage(
      gameId = gameId,
      action = "attack",
      parameters = Map(
        "fromTerritory" -> fromTerritory,
        "toTerritory" -> toTerritory,
        "troops" -> troops.toString,
        "defenderId" -> defenderId
      )
    )
    networkManager.sendMessage(action)
  }

  /**
   * Sends a request to defend a territory under attack.
   *
   * @param gameId The ID of the current game
   * @param territory The name of the territory to defend
   * @param troops The number of troops to use in defense
   * @return Future indicating whether the message was successfully sent
   */
  def defend(gameId: String, territory: String, troops: Int): Future[Boolean] = {
    val action = GameActionMessage(
      gameId = gameId,
      action = "defend",
      parameters = Map(
        "territory" -> territory,
        "troops" -> troops.toString
      )
    )
    networkManager.sendMessage(action)
  }

  /**
   * Sends a request to reinforce a territory by moving troops from another territory.
   *
   * @param gameId The ID of the current game
   * @param fromTerritory The territory to move troops from
   * @param toTerritory The territory to move troops to
   * @param troops The number of troops to move
   * @return Future indicating whether the message was successfully sent
   */
  def reinforce(gameId: String, fromTerritory: String, toTerritory: String, troops: Int): Future[Boolean] = {
    val action = GameActionMessage(
      gameId = gameId,
      action = "reinforce",
      parameters = Map(
        "from" -> fromTerritory,
        "to" -> toTerritory,
        "troops" -> troops.toString
      )
    )
    networkManager.sendMessage(action)
  }

  /**
   * Sends a request to end the current game phase.
   *
   * @param gameId The ID of the current game
   * @return Future indicating whether the message was successfully sent
   */
  def endPhase(gameId: String): Future[Boolean] = {
    val action = GameActionMessage(
      gameId = gameId,
      action = "end_phase",
      parameters = Map()
    )
    networkManager.sendMessage(action)
  }

  /**
   * Sends a request to end the current player's turn.
   *
   * @param gameId The ID of the current game
   * @return Future indicating whether the message was successfully sent
   */
  def endTurn(gameId: String): Future[Boolean] = {
    val action = GameActionMessage(
      gameId = gameId,
      action = "end_turn",
      parameters = Map()
    )
    networkManager.sendMessage(action)
  }

  /**
   * Sends a request to end the attack phase.
   *
   * @param gameId The ID of the current game
   * @return Future indicating whether the message was successfully sent
   */
  def endAttack(gameId: String): Future[Boolean] = {
    val action = GameActionMessage(
      gameId = gameId,
      action = "end_attack",
      parameters = Map()
    )
    networkManager.sendMessage(action)
  }
  
  /**
   * Sends a request to trade in a set of cards for bonus troops.
   *
   * @param gameId The ID of the current game
   * @param cardIds List of card IDs to trade in
   * @return Future indicating whether the message was successfully sent
   */
  def tradeCards(gameId: String, cardIds: List[String]): Future[Boolean] = {
    val action = GameActionMessage(
      gameId = gameId,
      action = "trade_cards",
      parameters = Map(
        "cards" -> cardIds.mkString(",")
      )
    )
    networkManager.sendMessage(action)
  }

  /**
   * Sends a request to end the setup phase of the game.
   *
   * @param gameId The ID of the current game
   * @return Future indicating whether the message was successfully sent
   */
  def endSetup(gameId: String): Future[Boolean] = {
    val action = GameActionMessage(
      gameId = gameId,
      action = "end_setup",
      parameters = Map()
    )
    networkManager.sendMessage(action)
  }
}