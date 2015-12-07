/* code patterns copied from https://github.com/jrudolph/akka-http-scala-js-websocket-chat*/
package websockets

import akka.actor._
import akka.http.scaladsl.model.ws.{ Message, TextMessage }
import akka.stream.stage._

import akka.http.scaladsl.server.Directives
import akka.stream.Materializer
import akka.stream.scaladsl.Flow

import upickle._

class Webservice(implicit fm: Materializer, system: ActorSystem) extends Directives {
  def route =
    get {
      pathSingleSlash {
        getFromResource("webapp/index.html")
      } ~
        path("game") {
          parameter('name) { name =>
            handleWebsocketMessages(websocketFlow(sender = name))
          }
        }
    } ~
      getFromResourceDirectory("webapp")

  def websocketFlow(sender: String): Flow[Message, Message, Unit] =
    Flow[Message]
      .collect { case TextMessage.Strict(msg) => game.UserInput(msg) } // unpack incoming WS text messages...
      .via(GameFlow.flow(sender)) // ... and route them through the gameFlow ...
      .map{ case b: game.Broadcast => TextMessage.Strict(write(b)) } // ... pack outgoing messages into WS JSON messages ...
      .via(reportErrorsFlow) // ... then log any processing errors on stdin

  def reportErrorsFlow[T]: Flow[T, T, Unit] =
    Flow[T]
      .transform(() => new PushStage[T, T] {
        def onPush(elem: T, ctx: Context[T]): SyncDirective = ctx.push(elem)

        override def onUpstreamFailure(cause: Throwable, ctx: Context[T]): TerminationDirective = {
          println(s"WS stream failed with $cause")
          super.onUpstreamFailure(cause, ctx)
        }
      })
}
