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
import akka.http.scaladsl.settings.ServerSettings
import scala.concurrent.duration._
import akka.http.scaladsl.model.ws.UpgradeToWebSocket


object RisikoServer:
  def main(args: Array[String]): Unit =
    
    implicit val system = ActorSystem("RiskServer")
    implicit val executionContext = system.dispatcher 

    val log = Logging(system, getClass.getName())
    
    val host = "localhost"
    val port = 8080
   
    //crea un nuovo attore e lo  registra nell'ActorSystem
    val gameManager = system.actorOf(GameManager.props, "game-Manager")
    //GameManager.props fornisce la configurazione per creare l'attore GameManager
    
    val serverSettings = ServerSettings(system)
      .withTimeouts(
        ServerSettings(system).timeouts
          .withIdleTimeout(Duration.Inf) 
      )
      .withWebsocketSettings(
        ServerSettings(system).websocketSettings
          .withPeriodicKeepAliveMaxIdle(Duration.Inf) 
      )

    log.info(s"Server WebSocket configurato con timeout infinito")

    // Definisce le route per il server
    val route =
      concat(
        
        path("health") 
        {
          get{ 
            complete(HttpEntity(ContentTypes.`application/json`, """{\"status\":\"ok\"}"""))
          }
        },
        
        path("ws"){
          //utilizza nuovo flow per ogni client 
          handleWebSocketMessages(WebSocketHandler(gameManager))
        },
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
            }
          )
        }
      )
    val bindingFuture = Http().newServerAt(host, port)
      .withSettings(serverSettings)
      .bind(route)
    println(s"Server online at http://$host:$port/\nPress RETURN to stop...")

   

    StdIn.readLine() 

    // aggiunge un hook di shutdown per fermare il server in modo pulito
    // e fare unbind delle connessioni
    scala.sys.addShutdownHook {
      bindingFuture
        .flatMap(_.unbind())
        .onComplete(_ => system.terminate()) // Shutdown the actor system
      println("Server stopped.")
    }
end RisikoServer



