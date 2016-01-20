package game

case class Player(coord: (Int, Int), vector: (Int, Int), faceDirection: FaceDirection)
case class Shape(tiles: Array[Array[Boolean]])
case class PackedShape(tiles: Array[String])

case object Tick
case class Delay(counter: Int)
case class UserInput(dir: String)
case class StepData(state: State, input: Option[UserInput])

sealed trait FaceDirection
object FaceDirection {
  object Straight extends FaceDirection
  object Right extends FaceDirection
  object Left extends FaceDirection
}

sealed trait StateMod
case class SetPlayerVector(mod: (Int, Int), faceDirection: FaceDirection = FaceDirection.Straight) extends StateMod
case class Zoom(in: Boolean) extends StateMod
case class SetPlayerCoord(mod: (Int, Int)) extends StateMod

object Step {
  private def stateDrivenMod(state: State): StateMod = {
    val player = state.player
    SetPlayerCoord((player.coord._1 + player.vector._1, player.coord._2 + player.vector._2))
  }

  private def moveStepInPixels(tilePixels: Int) = Const.moveStepInTiles * tilePixels

  private def inputDrivenModOpt(moveStepInPixels: Int, input: Option[UserInput]): Option[StateMod] = input.collect  {
    case UserInput("rightKeyDown") => SetPlayerVector((moveStepInPixels, 0), FaceDirection.Right)
    case UserInput("upKeyDown") => SetPlayerVector((0, -moveStepInPixels))
    case UserInput("leftKeyDown") => SetPlayerVector((-moveStepInPixels, 0), FaceDirection.Left)
    case UserInput("downKeyDown") => SetPlayerVector((0, moveStepInPixels))
    case UserInput("zoomin") => Zoom(true)
    case UserInput("zoomout") => Zoom(false)
    case ui: UserInput if ui.dir.endsWith("Up") => SetPlayerVector((0, 0))
  }

  def step(data: StepData): Seq[StateMod] = {
    val moveStep = moveStepInPixels(data.state.tilePixels)
    List(stateDrivenMod(data.state)) ++ inputDrivenModOpt(moveStep, data.input).toList
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
    case SetPlayerCoord(newCoord) =>
      this.copy(player = player.copy(coord = newCoord))
    case SetPlayerVector(newVector, faceDir) =>
      this.copy(player = player.copy(vector = newVector, faceDirection = faceDir))
    case Zoom(in) => this.copy(tilePixels =
      if (in) math.floor(tilePixels * Const.zoomFactor).toInt
      else math.floor(tilePixels / Const.zoomFactor).toInt)
  }
}

object State {
  def init: State = State(Player((0, 0), (0, 0), FaceDirection.Straight), 0, Const.initTilePixels)

  def iteration(state: State, input: Option[UserInput]): State = {
    val stepData = StepData(state, input)
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