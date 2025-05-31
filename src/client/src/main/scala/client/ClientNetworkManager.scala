package client

import akka.actor.typed.ActorSystem
import akka.actor.typed.scaladsl.Behaviors
import akka.http.scaladsl.Http
import akka.http.scaladsl.model._
import scala.concurrent.{ExecutionContext, Future}
import scala.util.{Success, Failure}

class ClientNetworkManager {
  implicit val system: ActorSystem[Nothing] = ActorSystem(Behaviors.empty, "risk-client")
  implicit val executionContext: ExecutionContext = system.executionContext
  
  private val serverHost = "localhost"
  private val serverPort = 8080
  private val baseUrl = s"http://$serverHost:$serverPort"
  
  def connect(): Future[Boolean] = {
    val request = HttpRequest(
      method = HttpMethods.GET,
      uri = s"$baseUrl/api"
    )
    
    val responseFuture = Http().singleRequest(request)
    
    responseFuture.map { response =>
      response.status match {
        case StatusCodes.OK => 
          println("Successfully connected to server")
          true
        case _ => 
          println(s"Failed to connect to server: ${response.status}")
          false
      }
    }.recover {
      case ex: Exception =>
        println(s"Error connecting to server: ${ex.getMessage}")
        false
    }
  }
  
  def shutdown(): Unit = {
    system.terminate()
  }
}
