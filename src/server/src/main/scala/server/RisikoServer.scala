package server

import akka.actor.ActorSystem
import akka.http.scaladsl.Http
import akka.http.scaladsl.model.{ContentTypes, HttpEntity}
import akka.http.scaladsl.server.Directives._
import akka.stream.ActorMaterializer
import scala.concurrent.{ExecutionContext, Future}
import scala.util.{Success, Failure}
import scala.io.StdIn
import akka.event.Logging


object RisikoServer:
  def main(args: Array[String]): Unit =
    
    implicit val system = ActorSystem("RiskServer")
    implicit val executionContext = system.dispatcher 

    val log = Logging(system, getClass.getName())
    
    val host = "localhost"
    val port = 8080
   
    //val gameManager = system.actorOf(GameManager.props, "game-Manager")

    
    //val webSocketHandler = WebSocketHandler(gameManager)

    // Definisce le route per il server
    val route =
      concat(
        //Endpoint di base per verifica server attivo
        path("health") //percorso URL esatto
        {
          get{ //complete viene usato per rispondere a una richiesta con contenuto JSON
            complete(HttpEntity(ContentTypes.`application/json`, """{\"status\":\"ok\"}"""))
          }
        },
        // WebSocket endpoint per la comunicazione in tempo reale
       /* path("ws"){
          handleWebSocketMessages(webSocketHandler.flow)
        },*/
        // API REST per la gestione del gioco
        pathPrefix("api") { //prefisso di percorso per avere sotto-percorsi
          concat(
            path("login"){
              post{
                entity(as[String]){ // entity estrae il corpo della richiesta come stringa
                  username =>
                    log.info(s"User '$username' logged in successfully.")
                    complete(HttpEntity(ContentTypes.`application/json`, 
                      s"""{"success": true, "playerId": "${java.util.UUID.randomUUID()}"}"""))
                }
              }
            },
            // Endpoint per la creazione di un gioco
            path("games"){
              get{
                complete(HttpEntity(ContentTypes.`application/json`, 
                  """[{"gameId": "1", "gameName": "Game 1", "creatorId": "player1"}]"""))
              }
            }
          )
        }
      )
    val bindingFuture = Http().newServerAt(host, port).bind(route)
    println(s"Server online at http://$host:$port/\nPress RETURN to stop...")

    StdIn.readLine() // Attende l'input da console per fermare il server

    // aggiunge un hook di shutdown per fermare il server in modo pulito
    // e fare unbind delle connessioni
    scala.sys.addShutdownHook {
      bindingFuture
        .flatMap(_.unbind())
        .onComplete(_ => system.terminate()) // Shutdown the actor system
      println("Server stopped.")
    }
end RisikoServer
    
  

