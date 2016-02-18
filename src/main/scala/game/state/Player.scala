package game.state

import game.calculations.{ScreenCommon, CollisionDetection, Terrain}

case class Player(onMap: (Int, Int), vector: (Int, Int), onScreen: (Int, Int), faceDirection: FaceDirection, onGround: Boolean) {
  val playerFigureLogicalCorners = List((0, 0), (1, 0), (0, 1), (1, 1), (0, 2), (1, 2))

  def movePlayerOnMapMod: SetPlayerMapCoord = SetPlayerMapCoord(ScreenCommon.applyVector(onMap, vector))

  def movePlayerOnScreenMod: SetPlayerScreenCoord = SetPlayerScreenCoord(ScreenCommon.applyVector(onScreen, vector))

  def isAtCollisionVector(tilePixels: Int): Option[(Int, Int)] = {
    if(vector._1 > 0 || vector._2 > 0) { //TODO refactor
      val playerFigureCorners = playerFigureLogicalCorners.map { case (x, y) => (onMap._1 + x * tilePixels, onMap._2 + y * tilePixels) }
      val collisionVectors = playerFigureCorners.flatMap(CollisionDetection(tilePixels).detectCollision(_, vector))
      if (collisionVectors.isEmpty) None else Some(collisionVectors.min)
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
      copy(vector = newVector)
    case SetStandingOnGround(newOnGround) =>
      copy(onGround = newOnGround)
  }
}

