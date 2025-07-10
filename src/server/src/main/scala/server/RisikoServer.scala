package server

import akka.actor.ActorSystem
import akka.http.scaladsl.Http
import akka.http.scaladsl.model.HttpEntity
import akka.http.scaladsl.model.ContentTypes 
import akka.http.scaladsl.server.Directives._
import akka.stream.ActorMaterializer
import scala.concurrent.ExecutionContext
import scala.concurrent.Future
import scala.util.Failure
import scala.util.Success
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
    val gameManager = system.actorOf(GameManager.props, "game-Manager")
    val serverSettings = ServerSettings(system)
      .withTimeouts(
        ServerSettings(system).timeouts
          .withIdleTimeout(Duration.Inf) 
      )
      .withWebsocketSettings(
        ServerSettings(system).websocketSettings
          .withPeriodicKeepAliveMaxIdle(Duration.Inf) 
      )

    val route =
      concat(
        path("health") 
        {
          get{ 
            complete(HttpEntity(ContentTypes.`application/json`, """{\"status\":\"ok\"}"""))
          }
        },
        path("ws"){
          handleWebSocketMessages(WebSocketHandler(gameManager))
        },
        pathPrefix("api") { 
          concat(
            path("login"){
              post{
                entity(as[String]){ 
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
    scala.sys.addShutdownHook {
      bindingFuture
        .flatMap(_.unbind())
        .onComplete(_ => system.terminate()) 
      println("Server stopped.")
    }
end RisikoServer