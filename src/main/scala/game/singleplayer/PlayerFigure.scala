package game.singleplayer

import game.Const
import game.calculations.{Tunnels, ScreenCommon, CollisionDetection, Terrain}

case class PlayerDebugInfo(collisionLimitedVector: Option[(Int, Int)])

case class PlayerFigure(
  onMap: (Int, Int),
  vector: (Int, Int),
  onScreen: (Int, Int),
  faceDirection: FaceDirection,
  onGround: Boolean,
  tilePixels: Int,
  debugInfo: PlayerDebugInfo
) {
  val playerFigureLogicalCorners = List((0, 0), (1, 0), (0, 1), (1, 1), (0, 2), (1, 2))

  def movePlayerOnMapMod: SetPlayerMapCoord = SetPlayerMapCoord(ScreenCommon.applyVector(onMap, vector))

  def movePlayerOnScreenMod: SetPlayerScreenCoord = SetPlayerScreenCoord(ScreenCommon.applyVector(onScreen, vector))

  def isAtCollisionVector(terrain: Terrain): Option[(Int, Int)] = {
    def collisionVectorHypothesis(hypotheticalVector: (Int, Int)): Option[(Int, Int)] = {
      if(hypotheticalVector != (0, 0)) {
        val playerFigureCorners = playerFigureLogicalCorners.map { case (x, y) => (onMap._1 + x * tilePixels, onMap._2 + y * tilePixels) }
        val collisionVectors = playerFigureCorners.flatMap(CollisionDetection(terrain, tilePixels).detectCollision(_, hypotheticalVector))
        if (collisionVectors.isEmpty)
          None
        else
          Some(ScreenCommon.minimumVector(collisionVectors))
      }
      else None
    }
    def enforcedCollisionVectorHypothesis(hypotheticalVector: (Int, Int)): (Int, Int) =
      collisionVectorHypothesis(hypotheticalVector).getOrElse(hypotheticalVector)
    val straightOnTry = collisionVectorHypothesis(vector)
    lazy val verticalTry = Some(enforcedCollisionVectorHypothesis((0, vector._2)))
    lazy val horizontalTry = Some(enforcedCollisionVectorHypothesis((vector._1, 0)))
    Stream(straightOnTry, verticalTry, horizontalTry).find(!_.contains((0, 0))).getOrElse(straightOnTry)
  }

  def applyPlayerMod(mod: PlayerMod): PlayerFigure = mod match {
    case SetPlayerMapCoord(newOnMap) =>
      copy(onMap = newOnMap)
    case SetPlayerScreenCoord(newOnScreen) =>
      copy(onScreen = newOnScreen)
    case SetPlayerHorizontalVector(newVectorX, newFaceDir) =>
      copy(vector = (newVectorX, vector._2), faceDirection = newFaceDir)
    case SetPlayerVerticalVector(newVectorY) =>
      copy(vector = (vector._1, newVectorY))
    case SetPlayerVectorLimitedByCollision(newVector) =>
      copy(vector = newVector, debugInfo = PlayerDebugInfo(Some(newVector)))
    case SetStandingOnGround(newOnGround) =>
      copy(onGround = newOnGround)
    case ModifyPlayerVerticalVectorBy(acc) =>
      copy(vector = (vector._1, vector._2 + acc))
  }

  def moveToFreeSpotOnMap(terrain: Terrain) =
    this.copy(onMap =
      Stream.from(0).find(x =>
        this.copy(onMap = (x, 0), vector = (0, 1)).isAtCollisionVector(terrain).isEmpty
      ).map((_, 0)).getOrElse((0, 0)))

  def resetDebug: PlayerFigure = copy(debugInfo = PlayerDebugInfo(None))
}

object PlayerFigure {
  private def initPlayerOnScreen(tilePixels: Int): (Int, Int) =
    ScreenCommon(tilePixels).tileEven((Const.screenWidth / 2, Const.screenHeight / 2))

  def init(initTilePixels: Int, terrain: Terrain): PlayerFigure =
    PlayerFigure(
      onMap = (0, 0),
      vector = (0, 0),
      onScreen = initPlayerOnScreen(initTilePixels),
      faceDirection = FaceDirection.Straight,
      onGround = false,
      tilePixels = initTilePixels,
      debugInfo = PlayerDebugInfo(None)
    ).moveToFreeSpotOnMap(terrain)
}

