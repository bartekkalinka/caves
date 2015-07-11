package game

import akka.actor._
import scala.concurrent.duration._
import scala.concurrent.ExecutionContext.Implicits.global
import scala.util.Random

case object Tick
case class StateData(player: Player, other: Other, score: Score)
case class UserInput(dir: Option[String])
case class StepData(state: StateData, input: UserInput)

sealed trait OtherMod
case class NewOther(x: Int) extends OtherMod
case class OtherMove(yMod: Int) extends OtherMod
case class StateMod(playerXMod: Int, otherMod: OtherMod)

object Step {
  def props() = Props(classOf[Step])
}
class Step extends Actor with ActorLogging {
  //TODO future: layers of state modification, one triggering next
  def receive = {
    case StepData(state, input) => {
      log.debug("StepData received, input = " + input.dir.getOrElse("null"))
      val playerXMod = input.dir match {
          case Some ("right") => 1
          case Some("left") => -1
          case _ => 0
      }
      val otherMod = if(state.other.y > 0) OtherMove(-1) else NewOther(Random.nextInt(37))
      context.actorSelection("../state") ! StateMod(playerXMod, otherMod)
    }
  }
}

object Input {
  def props() = Props(classOf[Input])
}
class Input extends Actor with ActorLogging {
  var userInput: Option[String] = None

  def receive = {
    case UserInput(ui) => {
      //log.debug("UserInput " + ui.get + " received")
      userInput = ui
    }
    case sd: StateData => {
      log.debug("StateData received")
      context.actorSelection("../step") ! StepData(sd, UserInput(userInput))
      userInput = None
    }
  }
}

object State {
  def props() = Props(classOf[State])

  def otherModApply(other: Other, otherMod: OtherMod) = otherMod match {
    case NewOther(x) => Other(x, 25)
    case OtherMove(yMod) => other.copy(y = other.y + yMod)
  }
}
class State extends Actor with ActorLogging {
  var player = Player(0)
  var other = Other(15, -1)

  override def preStart(): Unit = { context.system.scheduler.schedule(1 seconds, 50 millis, self, Tick) }

  def receive = {
    case StateMod(playerXMod, otherMod) => {
      log.debug("StateMod received")
      //apply modifications
      player = player.copy(x = player.x + playerXMod)
      other = State.otherModApply(other, otherMod)

      //broadcast state to client
      context.actorSelection("../websocket") ! Broadcast(player, other)
    }

    case Tick => {
      log.debug("Tick received")
      context.actorSelection("../input") ! StateData(player, other)
    }
  }
}
