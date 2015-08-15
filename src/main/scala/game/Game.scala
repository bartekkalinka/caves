package game

import akka.actor._
import scala.concurrent.duration._
import scala.concurrent.ExecutionContext.Implicits.global
import scala.util.Random

case class Player(x: Int, y: Int)
case class Other(x: Int, y: Int)
case class Broadcast(player: Player, other: Other, score: Int, shape: Array[String])

case object Tick
case class Delay(counter: Int)
case class OtherState(state: Other, delay: Delay)
case class StateData(player: Player, other: OtherState)
case class UserInput(dir: Option[String])
case class StepData(state: StateData, input: UserInput)

sealed trait OtherMod
case class NewOther(x: Int) extends OtherMod
case class OtherMove(yMod: Int) extends OtherMod
case class PlayerMove(xMod: Int, yMod: Int)
case object DelayCount extends OtherMod
case class StateMod(playerMove: PlayerMove, otherMod: OtherMod, scoreMod: Int)

object Step {
  def props() = Props(classOf[Step])
}
class Step extends Actor with ActorLogging {
  def receive = {
    case StepData(state, input) => {
      log.debug("StepData received, input = " + input.dir.getOrElse("null"))
      val playerMove = input.dir match {
          case Some("right") => PlayerMove(1, 0)
          case Some("up") => PlayerMove(0, -1)
          case Some("left") => PlayerMove(-1, 0)
          case Some("down") => PlayerMove(0, 1)
          case _ => PlayerMove(0, 0)
      }
      val otherCaught = state.other.state.x == state.player.x && state.other.state.y == 1
      val otherAction = state.other.delay.counter == 0
      val otherMod =
        if(state.other.state.y <= 0 || otherCaught) {
          NewOther(Random.nextInt(37))
        } else {
          if(otherAction) OtherMove(-1) else DelayCount
        }
      val scoreMod = if(otherCaught) 1 else 0
      context.actorSelection("../state") ! StateMod(playerMove, otherMod, scoreMod)
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

  def otherModApply(other: OtherState, otherMod: OtherMod) = otherMod match {
    case NewOther(x) => OtherState(Other(x, 25), Delay(1))
    case OtherMove(yMod) => OtherState(other.state.copy(y = other.state.y + yMod), Delay(1))
    case DelayCount => other.copy(delay = Delay(0))
  }
}
class State extends Actor with ActorLogging {
  var player = Player(15, 0)
  var other = OtherState(Other(15, -1), Delay(1))
  var score = 0
  var terrainCoord = (0, 0)

  override def preStart(): Unit = { context.system.scheduler.schedule(1 seconds, 50 millis, self, Tick) }

  def receive = {
    case StateMod(playerMove, otherMod, scoreMod) => {
      log.debug("StateMod received")
      //apply modifications
      player = player.copy(x = player.x + playerMove.xMod, y = player.y + playerMove.yMod)
      other = State.otherModApply(other, otherMod)
      score += scoreMod
      val terrain = (ShapeGenWrapper.get _).tupled(terrainCoord)

      //broadcast state to client
      context.actorSelection("../websocket") ! Broadcast(player, other.state, score, terrain)
    }

    case Tick => {
      log.debug("Tick received")
      context.actorSelection("../input") ! StateData(player, other)
    }
  }
}
