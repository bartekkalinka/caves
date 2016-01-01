package game

case class Player(x: Int, y: Int)
case class Shape(tiles: Array[Array[Boolean]])
case class PackedShape(tiles: Array[String])

case object Tick
case class Delay(counter: Int)
case class StateData(player: Player, moveStepInPixels: Int)
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
      case UserInput("right") => PlayerMove(data.state.moveStepInPixels, 0)
      case UserInput("up") => PlayerMove(0, -data.state.moveStepInPixels)
      case UserInput("left") => PlayerMove(-data.state.moveStepInPixels, 0)
      case UserInput("down") => PlayerMove(0, data.state.moveStepInPixels)
      case UserInput("zoomin") => Zoom(true)
      case UserInput("zoomout") => Zoom(false)
    }
    StateMod(inputDrivenModOpt)
  } 
}

object Const {
  private val baseTilesPerShape = 6
  private val targetNoiseDetail = 3
  private val baseMultiplier = Math.pow(2, Const.targetNoiseDetail).toInt
  val tilesPerShape = baseTilesPerShape * baseMultiplier
  val initTilePixels = 64 / baseMultiplier
  val moveStepInTiles = 1
  val zoomFactor = Math.sqrt(1.5)
  val screenWidth = 768
  val screenHeight = 500
}

case class State(player: Player, score: Int, tilePixels: Int)
{
  def applyMod(mod: StateMod): State = {
    mod.inputDrivenModOpt.map {
      case PlayerMove(xMod, yMod) => copy(player = player.copy(x = player.x + xMod, y = player.y + yMod))
      case Zoom(in) => copy(tilePixels =
        if(in) math.floor(tilePixels * Const.zoomFactor).toInt
        else math.floor(tilePixels / Const.zoomFactor).toInt)
    }.getOrElse(this)
  }
}

object State {
  def init: State = State(Player(0, 0), 0, Const.initTilePixels)

  def iteration(state: State, input: Option[UserInput]): State = {
    val stepData = StepData(StateData(state.player, Const.moveStepInTiles * state.tilePixels), input)
    state.applyMod(Step.step(stepData))
  }
}

case class Broadcast(player: Player, tilePixels: Int, shape: PackedShape)

object Broadcast
{
  private def packShape(shape: Shape): PackedShape = PackedShape(shape.tiles.map(_.map(if(_) "1" else "0").reduce(_ + _)))

  def fromState(state: State): Broadcast = {
    val shape = Screen.calculate(state.player, state.tilePixels)
    Broadcast(state.player, state.tilePixels, packShape(shape))
  }
}