package game.state

import game.calculations.Terrain

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

