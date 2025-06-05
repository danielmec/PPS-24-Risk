package client

import scala.concurrent.Await
import scala.concurrent.duration._
import scala.util.{Success, Failure}
import scala.io.StdIn
import spray.json._
import ClientJsonSupport._

/**
 * Punto di ingresso dell'applicazione client Risiko -- TEST
 */
object ClientApp:
  def main(args: Array[String]): Unit =
    val client = new ClientNetworkManager()
    
    println("Avvio login...")
    println("Inserisci il nome del giocatore:")
    
    // Leggi il nome utente da tastiera
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
    
    // Attende il completamento del login e gestisce il risultato
    loginFuture.onComplete {
      case Success(true) => 
        println(s"Hai completato il login! ID giocatore: ${client.getPlayerId.getOrElse("sconosciuto")}")
        // Connessione al WebSocket dopo il login
        connectWebSocket(client)
        
      case Success(false) => 
        println("Login fallito")
      case Failure(ex) => 
        println(s"Errore durante il login: ${ex.getMessage}")
    }(client.executionContext)
    
    // Attendi il completamento prima di terminare
    println("Premi Invio per terminare il client...")
    StdIn.readLine()
    client.shutdown()
    println("Client terminato")

  def connectWebSocket(client: ClientNetworkManager): Unit =
    
    implicit val ec = client.executionContext  
    
    println("Connessione al WebSocket...")

    client.connectWebSocket().onComplete {
      case Success(true) => 
        println("Connessione WebSocket stabilita con successo")
        
        // Usa ClientJsonSupport per creare il messaggio JSON
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
