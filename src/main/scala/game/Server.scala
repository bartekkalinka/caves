package game

import akka.actor._
import akka.http.scaladsl.Http
import akka.stream.ActorMaterializer
import scala.util.{ Success, Failure }
import websockets._
import scala.concurrent.ExecutionContext.Implicits.global

object WebSocketServer {
  def props() = Props(classOf[WebSocketServer])
}
class WebSocketServer extends Actor with ActorLogging {
  /* code patterns copied from https://github.com/jrudolph/akka-http-scala-js-websocket-chat*/
  var subscribers = Set.empty[ActorRef]

  def receive: Receive = {
    case NewParticipant(name, subscriber) =>
      context.watch(subscriber)
      subscribers += subscriber
      log.debug(s"$name joined!")
    case x: UserInput => {
      //log.debug("UserInput " + msg.get + " received")
      context.actorSelection("../input") ! x
    }
    case b: Broadcast => subscribers.foreach(_ ! b)
    case ParticipantLeft(person) => log.debug(s"$person left!")
    case Terminated(sub)         => subscribers -= sub // clean up dead subscribers
  }
}

object Main {
  def main(args: Array[String]): Unit = {
    implicit val system = ActorSystem()
    implicit val materializer = ActorMaterializer()

    val server = system.actorOf(WebSocketServer.props(), "websocket")
    system.actorOf(Step.props(), "step")
    system.actorOf(Input.props(), "input")
    system.actorOf(StateHolder.props(), "state")

    val interface = "localhost"
    val port = 8080
    val service = new Webservice(server)
    val binding = Http().bindAndHandle(service.route, interface, port)

    binding.onComplete {
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
