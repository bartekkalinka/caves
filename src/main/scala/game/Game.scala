package game

case class Player(onMap: (Int, Int), vector: (Int, Int), onScreen: (Int, Int), faceDirection: FaceDirection) {
  private def applyVector(coord: (Int, Int), vector: (Int, Int)): (Int, Int) =
    (coord._1 + vector._1, coord._2 + vector._2)

  def movePlayerOnMapMod: SetPlayerMapCoord = SetPlayerMapCoord(applyVector(onMap, vector))

  def movePlayerOnScreenMod: SetPlayerScreenCoord = SetPlayerScreenCoord(applyVector(onScreen, vector))

  def isAtCollision(tilePixels: Int): Boolean = {
    val terrain = Terrain(tilePixels)
    terrain.isTileSet(onMap) || terrain.isTileSet((onMap._1, onMap._2 + tilePixels))
  }

  def applyPlayerMod(mod: PlayerMod): Player = mod match {
    case SetPlayerMapCoord(newOnMap) =>
      copy(onMap = newOnMap)
    case SetPlayerScreenCoord(newOnScreen) =>
      copy(onScreen = newOnScreen)
    case SetPlayerVector(newVector, newFaceDir) =>
      copy(vector = newVector, faceDirection = newFaceDir)
  }
}

case class Shape(tiles: Array[Array[Boolean]])
case class PackedShape(tiles: Array[String])
case class UserInput(dir: String)
case class StepData(state: State, input: Option[UserInput])

sealed trait FaceDirection
object FaceDirection {
  object Straight extends FaceDirection
  object Right extends FaceDirection
  object Left extends FaceDirection
}

sealed trait StateMod
case class Zoom(in: Boolean) extends StateMod
sealed trait PlayerMod extends StateMod
case class SetPlayerVector(mod: (Int, Int), faceDirection: FaceDirection = FaceDirection.Straight) extends PlayerMod
case class SetPlayerMapCoord(onMap: (Int, Int)) extends PlayerMod
case class SetPlayerScreenCoord(onScreen: (Int, Int)) extends PlayerMod

object Step {
  private def stateDrivenMod(state: State): List[StateMod] = {
    val possibleMod = state.player.movePlayerOnMapMod
    if(!state.player.applyPlayerMod(possibleMod).isAtCollision(state.tilePixels)) {
      List(possibleMod, SetPlayerScreenCoord(movePlayerOnScreenIfStaysInTheMiddle(state.player)))
    }
    else {
      List[StateMod]()
    }
  }

  private def movePlayerOnScreenIfStaysInTheMiddle(player: Player): (Int, Int) = {
    val newPos = player.movePlayerOnScreenMod.onScreen
    if(Screen.isInTheMiddleOfScreen(newPos)) newPos else player.onScreen
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
    stateDrivenMod(data.state) ++ inputDrivenModOpt(moveStep, data.input).toList
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
    case playerMod: PlayerMod => this.copy(player = player.applyPlayerMod(playerMod))
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