package game

import akka.actor._
import scala.concurrent.duration._
import scala.concurrent.ExecutionContext.Implicits.global
import scala.util.Random

case class Player(x: Int, y: Int)
case class Shape(dx: Int, dy: Int, tiles: Array[String])
case class Broadcast(player: Player, shapes: Array[Shape])

case object Tick
case class Delay(counter: Int)
case class StateData(player: Player)
case class UserInput(dir: Option[String])
case class StepData(state: StateData, input: UserInput)

sealed trait OtherMod
case class NewOther(x: Int) extends OtherMod
case class OtherMove(yMod: Int) extends OtherMod
case class PlayerMove(xMod: Int, yMod: Int)
case object DelayCount extends OtherMod
case class StateMod(playerMove: PlayerMove)

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
      context.actorSelection("../state") ! StateMod(playerMove)
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
}
class State extends Actor with ActorLogging {
  var player = Player(15, 0)
  var score = 0
  val terrainCoords = List((0, 0), (0, 1), (1, 0), (1, 1))

  override def preStart(): Unit = { context.system.scheduler.schedule(1 seconds, 50 millis, self, Tick) }

  def receive = {
    case StateMod(playerMove) => {
      log.debug("StateMod received")
      //apply modifications
      player = player.copy(x = player.x + playerMove.xMod, y = player.y + playerMove.yMod)
      val terrain = terrainCoords.map(coord => Shape(coord._1, coord._2, (ShapeGenWrapper.get _).tupled(coord))).toArray

      //broadcast state to client
      context.actorSelection("../websocket") ! Broadcast(player, terrain)
    }

    case Tick => {
      log.debug("Tick received")
      context.actorSelection("../input") ! StateData(player)
    }
  }
}
