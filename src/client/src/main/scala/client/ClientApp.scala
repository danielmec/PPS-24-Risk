package client

import scala.concurrent.Await
import scala.concurrent.duration._
import scala.util.{Success, Failure}
import scala.io.StdIn

/**
 * Punto di ingresso dell'applicazione client Risiko
 */
object ClientApp:
  def main(args: Array[String]): Unit =
    val client = new ClientNetworkManager()
    
    println("Avvio login...")
    println("Inserisci il nome del giocatore:")
    
    // Leggi il nome utente da tastiera
    val username = StdIn.readLine().trim
    
    val isNameEmpty: String => Boolean = name => name.trim.isEmpty()

    username  match
      case name if isNameEmpty(name)  =>
        println("Nome utente non valido. Uscita...")
        client.shutdown()
        return
      case validName => 
        println(s"avvio")

    println(s"Tentativo di login come '$username'...")
    val loginFuture = client.login(username)
    
    loginFuture.onComplete {
      case Success(true) => 
        println(s"Hai completato il login! ID giocatore: ${client.getPlayerId.getOrElse("sconosciuto")}")
      case Success(false) => 
        println("Login fallito")
      case Failure(ex) => 
        println(s"Errore durante il login: ${ex.getMessage}")
    }(client.executionContext)
    
    // Attendi il completamento prima di terminare
    Thread.sleep(5000)
    client.shutdown()
    println("Client terminato")

end ClientApp
