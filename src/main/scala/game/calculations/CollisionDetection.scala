package game.calculations

case class CollisionDetection(tilePixels: Int) {
  //TODO return shortened vector after impact when colliding
  def detectCollision(pointBeforeMovement: (Int, Int), vectorToMove: (Int, Int)): Option[(Int, Int)] = {
    val pointAfterMovement = ScreenCommon.applyVector(pointBeforeMovement, vectorToMove)
    if(Terrain(tilePixels).isTileSet(pointAfterMovement)) Some((0, 0)) else None
  }
}
