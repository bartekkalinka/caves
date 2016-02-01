package game

case class Player(onMap: (Int, Int), vector: (Int, Int), onScreen: (Int, Int), faceDirection: FaceDirection) {
  private def applyVector(coord: (Int, Int), vector: (Int, Int)): (Int, Int) =
    (coord._1 + vector._1, coord._2 + vector._2)

  def movePlayerOnMap: Player = copy(onMap = applyVector(onMap, vector))

  def movePlayerOnScreen: Player =
    copy(onScreen = applyVector(onScreen, vector))

  def isAtCollision(tilePixels: Int): Boolean = {
    val terrain = Terrain(tilePixels)
    terrain.isTileSet(onMap) || terrain.isTileSet((onMap._1, onMap._2 + tilePixels))
  }

}
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
  private def stateDrivenMod(state: State): Option[StateMod] = {
    val possiblePosition = state.player.movePlayerOnMap
    if(!possiblePosition.isAtCollision(state.tilePixels)) {
      Some(SetPlayerCoord(possiblePosition.onMap,
        movePlayerOnScreenIfStaysInTheMiddle(state.player)))
    }
    else {
      None
    }
  }

  private def isInTheMiddleOfScreen(coord: (Int, Int)): Boolean =
    math.abs(coord._1 - Const.screenWidth / 2) < Const.screenWidth / 4 &&
    math.abs(coord._2 - Const.screenHeight / 2) < Const.screenHeight / 4

  private def movePlayerOnScreenIfStaysInTheMiddle(player: Player): (Int, Int) = {
    val newPos = player.movePlayerOnScreen.onScreen
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
    stateDrivenMod(data.state).toList ++ inputDrivenModOpt(moveStep, data.input).toList
  } 
}

object Const {
  private val baseTilesPerShape = 6
  private val targetNoiseDetail = 3
  private val baseMultiplier = Math.pow(2, Const.targetNoiseDetail).toInt
  val tilesPerShape = baseTilesPerShape * baseMultiplier
  val initTilePixels = 96 / baseMultiplier
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
    case Zoom(in) =>
      val newTilePixels =
        if (in) math.floor(tilePixels * Const.zoomFactor).toInt
        else math.floor(tilePixels / Const.zoomFactor).toInt
      val newPlayerOnScreen = State.tileEven(newTilePixels, player.onScreen)
      this.copy(tilePixels = newTilePixels, player = player.copy(onScreen = newPlayerOnScreen))
  }
}

object State {
  def tileEven(tilePixels: Int, coord: (Int, Int)) = (coord._1 - coord._1 % tilePixels, coord._2 - coord._2 % tilePixels)

  private def initPlayerOnScreen(tilePixels: Int): (Int, Int) =
    tileEven(tilePixels, (Const.screenWidth / 2, Const.screenHeight / 2))

  private def initPlayerOnMap(tilePixels: Int): (Int, Int) =
    Stream.from(0).find(x => !Terrain(tilePixels).isTileSet((x, 0))).map((_, 0)).getOrElse((0, 0))

  def init: State = State(
    player = Player(
      onMap = initPlayerOnMap(Const.initTilePixels),
      vector = (0, 0),
      onScreen = initPlayerOnScreen(Const.initTilePixels),
      faceDirection = FaceDirection.Straight
    ),
    score = 0,
    tilePixels = Const.initTilePixels
  )

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