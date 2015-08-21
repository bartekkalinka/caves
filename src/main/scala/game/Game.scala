package game

import akka.actor._
import scala.concurrent.duration._
import scala.concurrent.ExecutionContext.Implicits.global
import scala.util.Random

case class Player(x: Int, y: Int)
case class Shape(dx: Int, dy: Int, tiles: Array[String])
case class ScreenOffset(x: Int, y: Int)
case class Broadcast(player: Player, screen: ScreenOffset, shapes: Array[Shape])

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
          case Some("right") => PlayerMove(Const.moveStep, 0)
          case Some("up") => PlayerMove(0, -Const.moveStep)
          case Some("left") => PlayerMove(-Const.moveStep, 0)
          case Some("down") => PlayerMove(0, Const.moveStep)
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

object Const {
  val initTilePixels = 64
  val initShapeTiles = 6
  val shapePixels = initTilePixels * initShapeTiles
  val moveStep = 15
}

object Screen {
  def calculate(player: Player): (Array[Shape], ScreenOffset) = {
    val sh = Const.shapePixels
    val (shapeOffs, screenOffs) = player match { case Player(x, y) => ((x / sh, y / sh), ScreenOffset(x % sh, y % sh))}
    val terrainMatrix = Seq.tabulate(4, 4)((x, y) => (x, y)).flatten
    val terrainCoords = shapeOffs match { case (dx, dy) => terrainMatrix.map { case (x, y) => (x + dx, y + dy)} }
    val terrain = terrainCoords.map { case (dx, dy) => Shape(dx - shapeOffs._1, dy - shapeOffs._2, ShapeGenWrapper.get(dx, dy)) }
    (terrain.toArray, screenOffs)
  }
}

object State {
  def props() = Props(classOf[State])
}
class State extends Actor with ActorLogging {
  var player = Player(15, 0)
  var score = 0

  override def preStart(): Unit = { context.system.scheduler.schedule(1 seconds, 50 millis, self, Tick) }

  def receive = {
    case StateMod(playerMove) => {
      log.debug("StateMod received")
      //apply modifications
      player = player.copy(x = player.x + playerMove.xMod, y = player.y + playerMove.yMod)

      //broadcast state to client
      val (terrain, screenOffset) = Screen.calculate(player)
      context.actorSelection("../websocket") ! Broadcast(player, screenOffset, terrain)
    }

    case Tick => {
      log.debug("Tick received")
      context.actorSelection("../input") ! StateData(player)
    }
  }
}
