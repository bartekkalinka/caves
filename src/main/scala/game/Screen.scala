package game

object Screen {
  def pixelsPerShape(tilePixels: Int) = tilePixels * Const.tilesPerShape

  def cutDisplayed(terrain: Map[(Int, Int), Shape], screenOffs: ScreenOffset, tilePixels: Int): Shape = {
    val ScreenOffset(offx, offy) = screenOffs
    val (tileOffsX, tileOffsY) = (offx / tilePixels, offy / tilePixels)
    val piece00: Array[Array[Boolean]] = terrain.get((0, 0)).get.tiles
    val piece01: Array[Array[Boolean]] = terrain.get((0, 1)).get.tiles
    val piece10: Array[Array[Boolean]] = terrain.get((1, 0)).get.tiles
    val piece11: Array[Array[Boolean]] = terrain.get((1, 1)).get.tiles
    Shape(
      piece00.drop(tileOffsX).map(_.drop(tileOffsY)).zip(piece01.drop(tileOffsX).map(_.take(tileOffsY))).map(r => r._1 ++ r._2) ++
        piece10.take(tileOffsX).map(_.drop(tileOffsY)).zip(piece11.take(tileOffsX).map(_.take(tileOffsY))).map(r => r._1 ++ r._2)
    )
  }

  def absModulo(a: Int, b: Int) = if(a < 0) b + a % b else a % b

  def fluentDiv(a: Int, b: Int) = if(a < 0) a / b - 1 else a / b

  def calculate(player: Player, tilePixels: Int): Shape = {
    val pix = pixelsPerShape(tilePixels)
    val shapeCoord = (fluentDiv(player.x, pix), fluentDiv(player.y, pix))
    val screenOffs = ScreenOffset(absModulo(player.x, pix), absModulo(player.y, pix))
    val terrainMatrix = Seq.tabulate(4, 4)((x, y) => (x, y)).flatten
    val terrainCoords = shapeCoord match { case (dx, dy) => terrainMatrix.map { case (x, y) => (x + dx, y + dy)} }
    val terrain: Map[(Int, Int), Shape] = terrainCoords.map { case (dx, dy) =>
      ((dx - shapeCoord._1, dy - shapeCoord._2), Shape(ShapeGenWrapper.get(dx, dy))) }.toMap
    cutDisplayed(terrain, screenOffs, tilePixels)
  }
}



