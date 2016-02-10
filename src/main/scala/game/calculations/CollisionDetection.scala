package game.calculations

case class CollisionDetection(tilePixels: Int) {
  def setOfPointsIsAtCollisionWithTerrain(points: Seq[(Int, Int)]): Boolean =
    points.map(Terrain(tilePixels).isTileSet).reduce(_ || _)
}
