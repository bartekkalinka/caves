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

sealed trait StateMod
case class SetPlayerVector(xMod: Int, yMod: Int, faceDirection: FaceDirection = FaceDirection.Straight) extends StateMod
case class Zoom(in: Boolean) extends StateMod
case class SetPlayerCoord(xMod: Int, yMod: Int) extends StateMod

object Step {
  private def stateDrivenMod(state: StateData): StateMod = {
    val player = state.player
    SetPlayerCoord(player.x + player.vectorX, player.y + player.vectorY)
  }

  private def inputDrivenModOpt(state: StateData, input: Option[UserInput]): Option[StateMod] = input.collect  {
    case UserInput("rightKeyDown") => SetPlayerVector(state.moveStepInPixels, 0, FaceDirection.Right)
    case UserInput("upKeyDown") => SetPlayerVector(0, -state.moveStepInPixels)
    case UserInput("leftKeyDown") => SetPlayerVector(-state.moveStepInPixels, 0, FaceDirection.Left)
    case UserInput("downKeyDown") => SetPlayerVector(0, state.moveStepInPixels)
    case UserInput("zoomin") => Zoom(true)
    case UserInput("zoomout") => Zoom(false)
    case ui: UserInput if ui.dir.endsWith("Up") => SetPlayerVector(0, 0)
  }

  def step(data: StepData): Seq[StateMod] = {
    List(stateDrivenMod(data.state)) ++ inputDrivenModOpt(data.state, data.input).toList
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
  def applyMod(mod: StateMod): State = mod match {
    case SetPlayerCoord(xMod, yMod) =>
      this.copy(player = player.copy(
        x = xMod, y = yMod
      ))
    case SetPlayerVector(newX, newY, faceDir) =>
      this.copy(player = player.copy(vectorX = newX, vectorY = newY, faceDirection = faceDir))
    case Zoom(in) => this.copy(tilePixels =
      if (in) math.floor(tilePixels * Const.zoomFactor).toInt
      else math.floor(tilePixels / Const.zoomFactor).toInt)
  }
}

object State {
  def init: State = State(Player(0, 0, 0, 0, FaceDirection.Straight), 0, Const.initTilePixels)

  def iteration(state: State, input: Option[UserInput]): State = {
    val stepData = StepData(StateData(state.player, Const.moveStepInTiles * state.tilePixels), input)
    Step.step(stepData).foldLeft(state)((st, mod) => st.applyMod(mod))
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