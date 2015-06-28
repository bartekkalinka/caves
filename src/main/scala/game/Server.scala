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

final case class Push(msg: String)
final case class UserInput(msg: String)

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

    case x @ Push(msg) => workers.headOption.map { _ ! x }

    case x @ UserInput(msg) => context.actorSelection("../step") ! x
  }
}

object WebSocketWorker {
  def props(serverConnection: ActorRef) = Props(classOf[WebSocketWorker], serverConnection)
}
class WebSocketWorker(val serverConnection: ActorRef) extends HttpServiceActor
  with websocket.WebSocketServerWorker {
  override def receive = handshaking orElse businessLogicNoUpgrade orElse closeLogic

  def businessLogic: Receive = {
    case x @ (_: TextFrame) => context.actorSelection("..") ! UserInput(x.payload.utf8String)

    case Push(msg) => send(TextFrame(msg))

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

    //Actor creation and dependency injection
    val server = system.actorOf(WebSocketServer.props(), "websocket")
    val stepActor = system.actorOf(StepActor.props(), "step")

    IO(UHttp) ! Http.Bind(server, "localhost", 8080)

    Game.play(stepActor)

    system.shutdown()
    system.awaitTermination()
  }
}
