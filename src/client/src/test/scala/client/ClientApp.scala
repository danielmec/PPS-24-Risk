package test.client

import scala.concurrent.Await
import scala.concurrent.duration._
import scala.util.{Success, Failure}
import scala.io.StdIn
import spray.json._
import client.ClientJsonSupport._
import client.ClientNetworkManager

/**
 * Test client application entry point for the Risk game.
 * 
 * This object provides a simple command-line interface to test the client's
 * connectivity with the game server. It handles user authentication, WebSocket
 * connection, and basic message sending to verify functionality.
 */
object ClientApp:
  /**
   * Main entry point for the test client application.
   * 
   * Prompts the user for a username, attempts to log in to the server,
   * and establishes a WebSocket connection if login is successful.
   *
   * @param args Command-line arguments (not used)
   */
  def main(args: Array[String]): Unit =
    val client = new ClientNetworkManager()
    
    println("Avvio login...")
    println("Inserisci il nome del giocatore:")
    
    val username = StdIn.readLine().trim
    
    val isNameEmpty: String => Boolean = name => name.trim.isEmpty()

    username match
      case name if isNameEmpty(name) =>
        println("Nome utente non valido. Uscita...")
        client.shutdown()
        return
      case validName => 
        println(s"Avvio login per: $validName")

    println(s"Tentativo di login come '$username'...")
    val loginFuture = client.login(username)
    
    loginFuture.onComplete {
      case Success(true) => 
        println(s"Hai completato il login! ID giocatore: ${client.getPlayerId.getOrElse("sconosciuto")}")
        connectWebSocket(client)
        
      case Success(false) => 
        println("Login fallito")
      case Failure(ex) => 
        println(s"Errore durante il login: ${ex.getMessage}")
    }(client.executionContext)
    println("Premi Invio per terminare il client...")
    StdIn.readLine()
    client.shutdown()
    println("Client terminato")

  /**
   * Establishes a WebSocket connection to the game server and sends a test message.
   * 
   * This method connects to the WebSocket endpoint after successful authentication,
   * then sends a JoinLobbyMessage as a test to verify the connection is working.
   *
   * @param client The ClientNetworkManager instance to use for the connection
   */
  def connectWebSocket(client: ClientNetworkManager): Unit =
    
    implicit val ec = client.executionContext  
    
    println("Connessione al WebSocket...")

    client.connectWebSocket().onComplete {
      case Success(true) => 
        println("Connessione WebSocket stabilita con successo")
        val joinLobbyMsg = JoinLobbyMessage()
        val testMessage = toJson(joinLobbyMsg).toString
        
        client.sendWebSocketMessage(testMessage).foreach { sent =>
          if (sent) println(s"Messaggio di test inviato: $testMessage")
          else println("Impossibile inviare il messaggio di test")
        }
        
      case Success(false) => 
        println("Impossibile stabilire la connessione WebSocket")
        
      case Failure(ex) => 
        println(s"Errore durante la connessione al WebSocket: ${ex.getMessage}")
    } 

end ClientApp
