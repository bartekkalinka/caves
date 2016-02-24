package game.state

import game.Const
import game.calculations.{ScreenCommon, CollisionDetection, Terrain}

case class PlayerDebugInfo(collisionLimitedVector: Option[(Int, Int)])

case class Player(
  onMap: (Int, Int),
  vector: (Int, Int),
  onScreen: (Int, Int),
  faceDirection: FaceDirection,
  onGround: Boolean,
  debugInfo: PlayerDebugInfo
) {
  val playerFigureLogicalCorners = List((0, 0), (1, 0), (0, 1), (1, 1), (0, 2), (1, 2))

  def movePlayerOnMapMod: SetPlayerMapCoord = SetPlayerMapCoord(ScreenCommon.applyVector(onMap, vector))

  def movePlayerOnScreenMod: SetPlayerScreenCoord = SetPlayerScreenCoord(ScreenCommon.applyVector(onScreen, vector))

  private def isVectorACollisionVector(hypotheticalVector: (Int, Int), tilePixels: Int): Option[(Int, Int)] = {
    val playerFigureCorners = playerFigureLogicalCorners.map { case (x, y) => (onMap._1 + x * tilePixels, onMap._2 + y * tilePixels) }
    val collisionVectors = playerFigureCorners.flatMap(CollisionDetection(tilePixels).detectCollision(_, hypotheticalVector))
    if (collisionVectors.isEmpty) None else Some(collisionVectors.min)
  }

  def isAtCollisionVector(tilePixels: Int): Option[(Int, Int)] = {
    if(vector._1 > 0 || vector._2 > 0) {
      val straightOnTry = isVectorACollisionVector(vector, tilePixels)
//      lazy val verticalTry = isVectorACollisionVector((0, vector._2), tilePixels)
//      lazy val horizontalTry = isVectorACollisionVector((vector._1, 0), tilePixels)
//      Stream(straightOnTry, verticalTry, horizontalTry).find(!_.contains((0, 0))).getOrElse(straightOnTry)
      Stream(straightOnTry).find(!_.contains((0, 0))).getOrElse(straightOnTry)
    }
    else None
  }

  def applyPlayerMod(mod: PlayerMod): Player = mod match {
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

  def moveToFreeSpotOnMap(tilePixels: Int) =
    this.copy(onMap =
      Stream.from(0).find(x =>
        this.copy(onMap = (x, 0), vector = (0, 1)).isAtCollisionVector(tilePixels).isEmpty
      ).map((_, 0)).getOrElse((0, 0)))

  def resetDebug: Player = copy(debugInfo = PlayerDebugInfo(None))
}

object Player {
  private def initPlayerOnScreen(tilePixels: Int): (Int, Int) =
    ScreenCommon(tilePixels).tileEven((Const.screenWidth / 2, Const.screenHeight / 2))

  def initPlayer(tilePixels: Int): Player =
    Player(
      onMap = (0, 0),
      vector = (0, 0),
      onScreen = initPlayerOnScreen(tilePixels),
      faceDirection = FaceDirection.Straight,
      onGround = false,
      debugInfo = PlayerDebugInfo(None)
    ).moveToFreeSpotOnMap(tilePixels)
}

