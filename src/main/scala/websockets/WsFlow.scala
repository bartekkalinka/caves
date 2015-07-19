/* code patterns copied from https://github.com/jrudolph/akka-http-scala-js-websocket-chat*/
package websockets

import akka.actor._
import akka.stream.OverflowStrategy
import akka.stream.scaladsl._
import game.UserInput

case class NewParticipant(name: String, subscriber: ActorRef)
case class ParticipantLeft(name: String)

trait WsFlow {
  def wsFlow(sender: String): Flow[String, game.Broadcast, Unit]
}

object WsFlow {
  def create(wsActor: ActorRef): WsFlow = {
    def wsInSink(sender: String) = Sink.actorRef[UserInput](wsActor, ParticipantLeft(sender))

    new WsFlow {
      def wsFlow(sender: String): Flow[String, game.Broadcast, Unit] = {
        val in =
          Flow[String]
            .map(s => UserInput(Some(s)))
            .to(wsInSink(sender))

        val out =
          Source.actorRef[game.Broadcast](1, OverflowStrategy.fail)
            .mapMaterializedValue(wsActor ! NewParticipant(sender, _))

        Flow.wrap(in, out)(Keep.none)
      }
    }
  }
}