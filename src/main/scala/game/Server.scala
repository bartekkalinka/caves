package game

import akka.actor.{ ActorSystem, Actor, Props, ActorLogging, ActorRef, ActorRefFactory }
import akka.io.IO
import spray.can.Http
import spray.can.server.UHttp
import spray.can.websocket
import spray.can.websocket.frame.TextFrame
import spray.http.HttpRequest
import spray.can.websocket.FrameCommandFailed
import spray.routing.HttpServiceActor
import spray.json._


case class Player(x: Int)
case class Other(x: Int, y: Int)
case class Score(points: Int)
case class Broadcast(player: Player, other: Other, score: Score)

object BroadcastJsonProtocol extends DefaultJsonProtocol {
  implicit val playerFormat = jsonFormat1(Player.apply)
  implicit val otherFormat = jsonFormat2(Other.apply)
  implicit val scoreFormat = jsonFormat1(Score.apply)
  implicit val broadcastFormat = jsonFormat3(Broadcast.apply)
}


object WebSocketServer {
  def props() = Props(classOf[WebSocketServer])
}
class WebSocketServer extends Actor with ActorLogging {
  var workers: List[ActorRef] = List()

  def receive = {
    // when a new connection comes in we register a WebSocketConnection actor as the per connection handler
    case Http.Connected(remoteAddress, localAddress) =>
      val serverConnection = sender()
      log.info("connected")
      val workerName = "worker" + workers.length
      val worker = context.actorOf(WebSocketWorker.props(serverConnection), workerName)
      workers ::= worker
      log.info("register " + workerName)
      serverConnection ! Http.Register(worker)

    case x: Broadcast => workers.headOption.map { _ ! x }

    case x: UserInput => {
      //log.debug("UserInput " + msg.get + " received")
      context.actorSelection("../input") ! x
    }
  }
}

object WebSocketWorker {
  def props(serverConnection: ActorRef) = Props(classOf[WebSocketWorker], serverConnection)
}
class WebSocketWorker(val serverConnection: ActorRef) extends HttpServiceActor
  with websocket.WebSocketServerWorker {
  import BroadcastJsonProtocol._

  override def receive = handshaking orElse businessLogicNoUpgrade orElse closeLogic

  def businessLogic: Receive = {
    case x @ (_: TextFrame) => context.actorSelection("..") ! UserInput(Some(x.payload.utf8String))

    case x: Broadcast => send(TextFrame(x.toJson.toString()))

    case x: FrameCommandFailed =>
      log.error("frame command failed", x)

    case x: HttpRequest => // do something
  }

  def businessLogicNoUpgrade: Receive = {
    implicit val refFactory: ActorRefFactory = context
    runRoute {
      getFromResourceDirectory("webapp")
    }
  }
}

object Main {
  def main(args: Array[String]): Unit = {
    implicit val system = ActorSystem()

    val server = system.actorOf(WebSocketServer.props(), "websocket")
    val step = system.actorOf(Step.props(), "step")
    val input = system.actorOf(Input.props(), "input")
    val state = system.actorOf(State.props(), "state")

    IO(UHttp) ! Http.Bind(server, "localhost", 8080)

    system.awaitTermination()
  }
}
