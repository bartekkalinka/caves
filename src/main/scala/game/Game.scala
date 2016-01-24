package game

case class Player(onMap: (Int, Int), vector: (Int, Int), onScreen: (Int, Int), faceDirection: FaceDirection)
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
case class SetPlayerCoord(onMap: (Int, Int), onScreen: (Int, Int)) extends StateMod

object Step {
  private def stateDrivenMod(state: State): StateMod = {
    SetPlayerCoord(movePlayerOnMap(state.player),
      movePlayerOnScreenIfStaysInTheMiddle(state.player))
  }

  private def applyVector(coord: (Int, Int), vector: (Int, Int)): (Int, Int) =
    (coord._1 + vector._1, coord._2 + vector._2)

  private def movePlayerOnMap(player: Player): (Int, Int) =
    applyVector(player.onMap, player.vector)

  private def movePlayerOnScreen(player: Player): (Int, Int) =
    applyVector(player.onScreen, player.vector)

  private def isInTheMiddleOfScreen(coord: (Int, Int)): Boolean =
    math.abs(coord._1 - Const.screenWidth / 2) < Const.screenWidth / 4 &&
    math.abs(coord._2 - Const.screenHeight / 2) < Const.screenHeight / 4

  private def movePlayerOnScreenIfStaysInTheMiddle(player: Player): (Int, Int) = {
    val newPos = movePlayerOnScreen(player)
    if(isInTheMiddleOfScreen(newPos)) newPos else player.onScreen
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
    case SetPlayerCoord(onMap, onScreen) =>
      this.copy(player = player.copy(onMap = onMap, onScreen = onScreen))
    case SetPlayerVector(newVector, faceDir) =>
      this.copy(player = player.copy(vector = newVector, faceDirection = faceDir))
    case Zoom(in) => this.copy(tilePixels =
      if (in) math.floor(tilePixels * Const.zoomFactor).toInt
      else math.floor(tilePixels / Const.zoomFactor).toInt)
  }
}

object State {
  private def initPlayerOnScreen(tilePixels: Int): (Int, Int) = {
    def tileEven(coord: Int) = coord - coord % tilePixels
    (tileEven(Const.screenWidth / 2), tileEven(Const.screenHeight / 2))
  }

  def init: State = State(Player((0, 0), (0, 0), initPlayerOnScreen(Const.initTilePixels),
    FaceDirection.Straight), 0, Const.initTilePixels)

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
    val shape = Screen.calculate(state.player, state.tilePixels)
    val playerOnScreen = PlayerOnScreen.tupled(state.player.onScreen)
    Broadcast(playerOnScreen, state.player.faceDirection, state.tilePixels, packShape(shape))
  }
}