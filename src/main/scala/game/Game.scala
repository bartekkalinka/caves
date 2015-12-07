package game

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