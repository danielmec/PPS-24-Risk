package client

import scala.concurrent.{ExecutionContext, Future}
import client.ClientJsonSupport.GameActionMessage


class GameActionHandler(networkManager: ClientNetworkManager)(implicit ec: ExecutionContext) {

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

  def endPhase(gameId: String): Future[Boolean] = {
    val action = GameActionMessage(
      gameId = gameId,
      action = "end_phase",
      parameters = Map()
    )
    networkManager.sendMessage(action)
  }

  def endTurn(gameId: String): Future[Boolean] = {
    val action = GameActionMessage(
      gameId = gameId,
      action = "end_turn",
      parameters = Map()
    )
    networkManager.sendMessage(action)
  }


  def endAttack(gameId: String): Future[Boolean] = {
    val action = GameActionMessage(
      gameId = gameId,
      action = "end_attack",
      parameters = Map()
    )
    networkManager.sendMessage(action)
  }
  
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

  def endSetup(gameId: String): Future[Boolean] = {
    val action = GameActionMessage(
      gameId = gameId,
      action = "end_setup",
      parameters = Map()
    )
    networkManager.sendMessage(action)
  }
}