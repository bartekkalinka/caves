package game

import akka.actor._
import akka.http.scaladsl.Http
import akka.stream.ActorMaterializer
import scala.util.{ Success, Failure }
import websockets._
import scala.concurrent.ExecutionContext.Implicits.global

object Main {
  def main(args: Array[String]): Unit = {
    implicit val system = ActorSystem()
    implicit val materializer = ActorMaterializer()

    val interface = "localhost"
    val port = 8080
    val service = new Webservice()
    val bindingF = Http().bindAndHandle(service.route, interface, port)

    bindingF.onComplete {
      case Success(binding) ⇒
        val localAddress = binding.localAddress
        println(s"Server is listening on ${localAddress.getHostName}:${localAddress.getPort}")
        system.awaitTermination()
      case Failure(e) ⇒
        println(s"Binding failed with ${e.getMessage}")
        system.shutdown()
    }
  }
}
