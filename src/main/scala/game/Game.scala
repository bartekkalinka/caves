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
  val tilesPerShape = 6
  val moveStep = 15
  val zoomFactor = Math.sqrt(1.5)
}

object Screen {
  def pixelsPerShape(pixelsPerTile: Int) = pixelsPerTile * Const.tilesPerShape

  def cutDisplayed(terrain: Map[(Int, Int), Shape], screenOffs: ScreenOffset, pixelsPerTile: Int): Shape = {
    val ScreenOffset(offx, offy) = screenOffs
    val (tileOffsX, tileOffsY) = (offx / pixelsPerTile, offy / pixelsPerTile)
    val piece00: Array[String] = terrain.get((0, 0)).get.tiles
    val piece01: Array[String] = terrain.get((0, 1)).get.tiles
    val piece10: Array[String] = terrain.get((1, 0)).get.tiles
    val piece11: Array[String] = terrain.get((1, 1)).get.tiles
    Shape(
      piece00.drop(tileOffsY).map(_.substring(tileOffsX)).zip(piece10.drop(tileOffsX).map(_.substring(0, tileOffsX))).map(r => r._1 + r._2) ++
      piece01.take(tileOffsY).map(_.substring(tileOffsX)).zip(piece11.take(tileOffsX).map(_.substring(0, tileOffsX))).map(r => r._1 + r._2)
    )
  }

  def calculate(player: Player, pixelsPerTile: Int): (Shape, ScreenOffset) = {
    val pix = pixelsPerShape(pixelsPerTile)
    val (shapeCoord, screenOffs) = player match { case Player(x, y) => ((x / pix, y / pix), ScreenOffset(x % pix, y % pix))}
    val terrainMatrix = Seq.tabulate(4, 4)((x, y) => (x, y)).flatten
    val terrainCoords = shapeCoord match { case (dx, dy) => terrainMatrix.map { case (x, y) => (x + dx, y + dy)} }
    val terrain: Map[(Int, Int), Shape] = terrainCoords.map { case (dx, dy) => ((dx - shapeCoord._1, dy - shapeCoord._2), Shape(ShapeGenWrapper.get(dx, dy))) }.toMap
    (cutDisplayed(terrain, screenOffs, pixelsPerTile), ScreenOffset(0, 0))
  }
}

case class State(player: Player, score: Int, pixelsPerTile: Int)
{
  def applyMod(mod: StateMod): State = {
    mod.inputDrivenModOpt.map {
      case PlayerMove(xMod, yMod) => copy(player = player.copy(x = player.x + xMod, y = player.y + yMod))
      case Zoom(in) => copy(pixelsPerTile =
        if(in) math.floor(pixelsPerTile * Const.zoomFactor).toInt
        else math.floor(pixelsPerTile / Const.zoomFactor).toInt)
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
    val (shape, screenOffset) = Screen.calculate(state.player, state.pixelsPerTile)
    Broadcast(state.player, screenOffset, state.pixelsPerTile, shape)
  }
}