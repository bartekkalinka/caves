package game

object Terrain {
  def pixelsPerShape(tilePixels: Int) = tilePixels * Const.tilesPerShape

  def absModulo(a: Int, b: Int) = if(a < 0) b + a % b else a % b

  def fluentDiv(a: Int, b: Int) = if(a < 0) a / b - 1 else a / b

  def shapeCoordAndOffset(mapPixelCoord: (Int, Int), tilePixels: Int): ((Int, Int), (Int, Int)) = {
    val pixPerShape = Terrain.pixelsPerShape(tilePixels)
    val shapeCoord = (fluentDiv(mapPixelCoord._1, pixPerShape), fluentDiv(mapPixelCoord._2, pixPerShape))
    val shapePixelOffset = (absModulo(mapPixelCoord._1, pixPerShape), absModulo(mapPixelCoord._2, pixPerShape))
    (shapeCoord, shapePixelOffset)
  }

  def isTileSet(mapPixelCoord: (Int, Int), tilePixels: Int): Boolean = {
    val (shapeCoord, shapePixelOffset) = shapeCoordAndOffset(mapPixelCoord, tilePixels)
    val shape = ShapeGenWrapper.get(shapeCoord._1, shapeCoord._1)
    shape(shapePixelOffset._1)(shapePixelOffset._2)
  }

  def getTerrainFromGenerator(shapeCoord: (Int, Int), shapeSpan: (Int, Int)): Map[(Int, Int), Shape] = {
    val terrainMatrix = Seq.tabulate(shapeSpan._1 + 1, shapeSpan._2 + 1)((x, y) => (x, y)).flatten
    val terrainCoords = shapeCoord match { case (dx, dy) => terrainMatrix.map { case (x, y) => (x + dx, y + dy)} }
    terrainCoords.map { case (dx, dy) =>
      ((dx - shapeCoord._1, dy - shapeCoord._2), Shape(ShapeGenWrapper.get(dx, dy))) }.toMap
  }
}

