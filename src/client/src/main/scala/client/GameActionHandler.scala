package client

import scala.concurrent.{ExecutionContext, Future}
import client.ClientJsonSupport.GameActionMessage

/**
 * Gestisce l'invio di azioni di gioco al server
 * 
 * Questa classe utilizza ClientNetworkManager per inviare le azioni ma incapsula
 * la logica specifica delle azioni di gioco.
 */
class GameActionHandler(networkManager: ClientNetworkManager)(implicit ec: ExecutionContext) {
  
  /**
   * Invia un'azione di piazzamento truppe
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
   * Invia un'azione di attacco
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
   * Invia un'azione di difesa
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
   * Invia un'azione di rinforzo (spostamento truppe)
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
   * Termina la fase corrente
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
   * Termina il turno corrente
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
   * Termina la fase di attacco
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
   * Scambia carte territorio per ottenere truppe bonus
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
}