package game.multiplayer

import akka.actor.{Props, Actor}
import scala.concurrent.duration._
import scala.concurrent.ExecutionContext.Implicits.global

case object CleanUp
object DeleteIdleActor {
  def props() = Props(classOf[DeleteIdleActor])
}
class DeleteIdleActor extends Actor {
  override def preStart(): Unit = { context.system.scheduler.schedule(3 seconds, 3 seconds, self, CleanUp) }

  def receive = {
    case CleanUp =>
      GlobalState.deleteIdlePlayers()
  }
}

