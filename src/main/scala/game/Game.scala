package game

case class Player(x: Int, y: Int)
case class Shape(tiles: Array[String])
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
  val baseTilesPerShape = 6
  val targetNoiseDetail = 3
  val moveStep = 15
  val zoomFactor = Math.sqrt(1.5)
}

object Screen {
  def pixelsPerShape(basePixelsPerTile: Int) = basePixelsPerTile * Const.baseTilesPerShape

  def cutDisplayed(terrain: Map[(Int, Int), Shape], screenOffs: ScreenOffset, baseTilePixels: Int): Shape = {
    val pixelsPerTile = (baseTilePixels / Math.pow(2, Const.targetNoiseDetail)).toInt
    val ScreenOffset(offx, offy) = screenOffs
    val (tileOffsX, tileOffsY) = (offx / pixelsPerTile, offy / pixelsPerTile)
    val piece00: Array[String] = terrain.get((0, 0)).get.tiles
    val piece01: Array[String] = terrain.get((0, 1)).get.tiles
    val piece10: Array[String] = terrain.get((1, 0)).get.tiles
    val piece11: Array[String] = terrain.get((1, 1)).get.tiles
    Shape(
      piece00.drop(tileOffsX).map(_.substring(tileOffsY)).zip(piece01.drop(tileOffsX).map(_.substring(0, tileOffsY))).map(r => r._1 + r._2) ++
      piece10.take(tileOffsX).map(_.substring(tileOffsY)).zip(piece11.take(tileOffsX).map(_.substring(0, tileOffsY))).map(r => r._1 + r._2)
    )
  }

  def absModulo(a: Int, b: Int) = if(a < 0) b + a % b else a % b

  def calculate(player: Player, baseTilePixels: Int): (Shape, ScreenOffset) = {
    val pix = pixelsPerShape(baseTilePixels)
    val shapeCoord = (player.x / pix, player.y / pix)
    val screenOffs = ScreenOffset(absModulo(player.x, pix), absModulo(player.y, pix))
    val terrainMatrix = Seq.tabulate(4, 4)((x, y) => (x, y)).flatten
    val terrainCoords = shapeCoord match { case (dx, dy) => terrainMatrix.map { case (x, y) => (x + dx, y + dy)} }
    val terrain: Map[(Int, Int), Shape] = terrainCoords.map { case (dx, dy) =>
      ((dx - shapeCoord._1, dy - shapeCoord._2), Shape(ShapeGenWrapper.get(dx, dy))) }.toMap
    (cutDisplayed(terrain, screenOffs, baseTilePixels), ScreenOffset(0, 0))
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

case class Broadcast(player: Player, screen: ScreenOffset, baseTilePixels: Int, shape: Shape)

object Broadcast
{
  def fromState(state: State): Broadcast = {
    val (shape, screenOffset) = Screen.calculate(state.player, state.baseTilePixels)
    Broadcast(state.player, screenOffset, state.baseTilePixels, shape)
  }
}