package game

import akka.actor._
import scala.concurrent.duration._
import scala.concurrent.ExecutionContext.Implicits.global
import scala.util.Random

case class Player(x: Int, y: Int)
case class Shape(dx: Int, dy: Int, tiles: Array[String])
case class ScreenOffset(x: Int, y: Int)

case object Tick
case class Delay(counter: Int)
case class StateData(player: Player)
case class UserInput(dir: String)
case class StepData(state: StateData, input: Option[UserInput])

sealed trait InputDrivenMod
case class PlayerMove(xMod: Int, yMod: Int) extends InputDrivenMod
case class Zoom(in: Boolean) extends InputDrivenMod
sealed trait OtherMod
case class NewOther(x: Int) extends OtherMod
case class OtherMove(yMod: Int) extends OtherMod
case object DelayCount extends OtherMod
case class StateMod(inputDrivenModOpt: Option[InputDrivenMod])

object Step {
  def props() = Props(classOf[Step])
  def step(data: StepData): StateMod = {
    val inputDrivenModOpt = data.input.map  {
      case UserInput("right") => PlayerMove(Const.moveStep, 0)
      case UserInput("up") => PlayerMove(0, -Const.moveStep)
      case UserInput("left") => PlayerMove(-Const.moveStep, 0)
      case UserInput("down") => PlayerMove(0, Const.moveStep)
      case UserInput("zoomin") => Zoom(true)
      case UserInput("zoomout") => Zoom(false)
    }
    StateMod(inputDrivenModOpt)
  } 
}
class Step extends Actor with ActorLogging {
  def receive = {
    case data@StepData(state, input) => {
      log.debug("StepData received, input = " + input.map(_.dir).getOrElse("null"))
      context.actorSelection("../state") ! Step.step(data) 
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
      userInput = Some(ui)
    }
    case sd: StateData => {
      log.debug("StateData received")
      context.actorSelection("../step") ! StepData(sd, userInput.map(UserInput))
      userInput = None
    }
  }
}

object Const {
  val initShapeTiles = 6
  val moveStep = 15
  val zoomFactor = Math.sqrt(1.5)
}

object Screen {
  def shapePixels(baseTilePixels: Int) = baseTilePixels * Const.initShapeTiles

  def calculate(player: Player, baseTilePixels: Int): (Array[Shape], ScreenOffset) = {
    val sh = shapePixels(baseTilePixels)
    val (shapeOffs, screenOffs) = player match { case Player(x, y) => ((x / sh, y / sh), ScreenOffset(x % sh, y % sh))}
    val terrainMatrix = Seq.tabulate(4, 4)((x, y) => (x, y)).flatten
    val terrainCoords = shapeOffs match { case (dx, dy) => terrainMatrix.map { case (x, y) => (x + dx, y + dy)} }
    val terrain = terrainCoords.map { case (dx, dy) => Shape(dx - shapeOffs._1, dy - shapeOffs._2, ShapeGenWrapper.get(dx, dy)) }
    (terrain.toArray, screenOffs)
  }
}

case class State(player: Player, score: Int, baseTilePixels: Int)
{
  def applyMod(mod: StateMod): State = {
    mod.inputDrivenModOpt.map {
      case PlayerMove(xMod, yMod) => copy(player = player.copy(x = player.x + xMod, y = player.y + yMod))
      case Zoom(in) => copy(baseTilePixels =
        if(in) math.floor(baseTilePixels * Const.zoomFactor).toInt
        else math.floor(baseTilePixels / Const.zoomFactor).toInt)
    }.getOrElse(this)
  }


}

object State {
  def init: State = State(Player(15, 0), 0, 64)
}

case class Broadcast(player: Player, screen: ScreenOffset, baseTilePixels: Int, shapes: Array[Shape])

object Broadcast
{
  def fromState(state: State): Broadcast = {
    val (terrain, screenOffset) = Screen.calculate(state.player, state.baseTilePixels)
    Broadcast(state.player, screenOffset, state.baseTilePixels, terrain)
  }
}

object StateHolder {
  def props() = Props(classOf[StateHolder])
}
class StateHolder extends Actor with ActorLogging {
  var state = State.init

  override def preStart(): Unit = { context.system.scheduler.schedule(1 seconds, 50 millis, self, Tick) }

  def receive = {
    case mod: StateMod => {
      log.debug("StateMod received")
      //apply modifications
      state = state.applyMod(mod)

      //broadcast state to client

      context.actorSelection("../websocket") ! Broadcast.fromState(state)
    }

    case Tick => {
      log.debug("Tick received")
      context.actorSelection("../input") ! StateData(state.player)
    }
  }
}
