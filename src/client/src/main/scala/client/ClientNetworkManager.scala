package client

import akka.actor.typed.ActorSystem
import akka.actor.typed.scaladsl.Behaviors
import akka.http.scaladsl.Http
import akka.http.scaladsl.model._
import akka.http.scaladsl.unmarshalling.Unmarshal
import scala.concurrent.{ExecutionContext, Future}
import scala.util.{Success, Failure, Try}

/**
 * Gestore delle comunicazioni di rete per il client Risiko
 */
class ClientNetworkManager:
  // Inizializzazione dell'ActorSystem per Akka HTTP
  implicit val system: ActorSystem[Nothing] = ActorSystem(Behaviors.empty, "risk-client")
  implicit val executionContext: ExecutionContext = system.executionContext
  
  // Configurazione del server
  private val serverHost = "localhost"
  private val serverPort = 8080
  private val baseUrl = s"http://$serverHost:$serverPort"
  
  // Stato del client (immutabile, usando Option)
  private var playerId: Option[String] = None
  
  
  def login(username: String): Future[Boolean] =
    val request = HttpRequest(
      method = HttpMethods.POST,
      uri = s"$baseUrl/api/login",
      entity = HttpEntity(ContentTypes.`application/json`, s"""{"username": "$username"}""")
    )
    
    // Invia la richiesta e gestisce la risposta
    Http().singleRequest(request).flatMap { response =>
      response.status match
        case StatusCodes.OK =>
          // Estrae il corpo della risposta come stringa , successivamente si creeà un Parser dedicato per JSON
          Unmarshal(response.entity).to[String].map { jsonString =>
            // Semplice estrazione dell'ID del giocatore
            val idPattern = """"playerId":\s*"([^"]+)"""".r
            idPattern.findFirstMatchIn(jsonString) match
              case Some(m) => 
                val id = m.group(1)
                playerId = Some(id)
                println(s"Login effettuato con successo. ID: $id")
                true
              case None =>
                println("Impossibile estrarre l'ID dalla risposta")
                false
          }
        case _ =>
          println(s"Errore di connessione: ${response.status}")
          Future.successful(false)
    }.recover {
      case ex: Exception =>
        println(s"Errore di rete: ${ex.getMessage}")
        false
    }
  

  def getPlayerId: Option[String] = playerId

  def shutdown(): Unit =
    system.terminate()

end ClientNetworkManager
