package game

case class Player(x: Int, y: Int, vectorX: Int, vectorY: Int, faceDirection: FaceDirection)
case class Shape(tiles: Array[Array[Boolean]])
case class PackedShape(tiles: Array[String])

case object Tick
case class Delay(counter: Int)
case class StateData(player: Player, moveStepInPixels: Int)
case class UserInput(dir: String)
case class StepData(state: StateData, input: Option[UserInput])

sealed trait FaceDirection
object FaceDirection {
  object Straight extends FaceDirection
  object Right extends FaceDirection
  object Left extends FaceDirection
}

sealed trait InputDrivenMod
case class PlayerVector(xMod: Int, yMod: Int, faceDirection: FaceDirection = FaceDirection.Straight) extends InputDrivenMod
case class Zoom(in: Boolean) extends InputDrivenMod
case class OtherMod(defaultFaceDirection: FaceDirection = FaceDirection.Straight)
case class StateMod(inputDrivenModOpt: Option[InputDrivenMod], otherMod: OtherMod = OtherMod())

object Step {
  def step(data: StepData): StateMod = {
    val inputDrivenModOpt = data.input.collect  {
      case UserInput("rightKeyDown") => PlayerVector(data.state.moveStepInPixels, 0, FaceDirection.Right)
      case UserInput("upKeyDown") => PlayerVector(0, -data.state.moveStepInPixels)
      case UserInput("leftKeyDown") => PlayerVector(-data.state.moveStepInPixels, 0, FaceDirection.Left)
      case UserInput("downKeyDown") => PlayerVector(0, data.state.moveStepInPixels)
      case UserInput("zoomin") => Zoom(true)
      case UserInput("zoomout") => Zoom(false)
      case ui: UserInput if ui.dir.endsWith("Up") => PlayerVector(0, 0)
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
    val newState = this.copy(player = player.copy(
      x = player.x + player.vectorX, y = player.y + player.vectorY
    ))
    mod.inputDrivenModOpt.map {
      case PlayerVector(newX, newY, faceDir) =>
        newState.copy(player = newState.player.copy(vectorX = newX, vectorY = newY, faceDirection = faceDir))
      case Zoom(in) => newState.copy(tilePixels =
        if(in) math.floor(tilePixels * Const.zoomFactor).toInt
        else math.floor(tilePixels / Const.zoomFactor).toInt)
    }.getOrElse(newState)
  }
}

object State {
  def init: State = State(Player(0, 0, 0, 0, FaceDirection.Straight), 0, Const.initTilePixels)

  def iteration(state: State, input: Option[UserInput]): State = {
    val stepData = StepData(StateData(state.player, Const.moveStepInTiles * state.tilePixels), input)
    state.applyMod(Step.step(stepData))
  }
}

case class PlayerOnScreen(x: Int, y: Int)
case class Broadcast(player: PlayerOnScreen, faceDirection: FaceDirection, tilePixels: Int, shape: PackedShape)

object Broadcast
{
  private def packShape(shape: Shape): PackedShape = PackedShape(shape.tiles.map(_.map(if(_) "1" else "0").reduce(_ + _)))

  def fromState(state: State): Broadcast = {
    val (shape, playerOnScreen) = Screen.calculate(state.player, state.tilePixels)
    Broadcast(playerOnScreen, state.player.faceDirection, state.tilePixels, packShape(shape))
  }
}