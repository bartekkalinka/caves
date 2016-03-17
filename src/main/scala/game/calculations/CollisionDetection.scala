package game.calculations

case class CollisionDetection(tilePixels: Int) {
  def detectCollision(pointBeforeMovement: (Int, Int), vectorToMove: (Int, Int), tunnels: Tunnels): Option[(Int, Int)] = {
    val pointAfterMovement = ScreenCommon.applyVector(pointBeforeMovement, vectorToMove)
    if(!Terrain.isTileSet(pointAfterMovement, tilePixels, tunnels)) {
      None
    }
    else {
      val movementLine = bresenhamLineAlgorithm((0, 0), vectorToMove)
      movementLine.reverse.find { partialVector =>
        val pointAfterPartialMovement = ScreenCommon.applyVector(pointBeforeMovement, partialVector)
        !Terrain.isTileSet(pointAfterPartialMovement, tilePixels, tunnels)
      }.orElse(Some(0, 0))
    }
  }

  def bresenhamLineAlgorithm(from: (Int, Int), to: (Int, Int)): Seq[(Int, Int)] = {
    import scala.math.abs

    val dx = abs(to._1 - from._1)
    val dy = abs(to._2 - from._2)

    val sx = if (from._1 < to._1) 1 else -1
    val sy = if (from._2 < to._2) 1 else -1

    val start = (from, dx - dy)
    val infinite = Stream.iterate(start) {
      case ((x, y), err) =>
        val e2 = 2 * err
        val (err2, x2) = if (e2 > -dy) (err - dy, x + sx) else (err, x)
        val (err3, y2) = if(e2 < dx) (err2 + dx, y + sy) else (err2, y)
        ((x2, y2), err3)
    }
    infinite.map(_._1).takeWhile {
      case (x, y) =>
        sx*x <= sx*to._1 && sy*y <= sy*to._2
    }.toList
  }
}
