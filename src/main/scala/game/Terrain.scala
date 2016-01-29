package game

case class Terrain(tilePixels: Int) {
  def isTileSet(mapPixelCoord: (Int, Int)): Boolean = {
    val (shapeCoord, shapePixelOffset) = shapeCoordAndOffset(mapPixelCoord)
    val shape = ShapeGenWrapper.get(shapeCoord._1, shapeCoord._2)
    val shapeTilesOffset = Screen.pixelsToTilesOffset(shapePixelOffset, tilePixels)
    shape(shapeTilesOffset._1)(shapeTilesOffset._2)
  }

  def getSliceWithCutParams(upperLeftPixelCoord: (Int, Int), lowerRightPixelCoord: (Int, Int)): TerrainSliceWithCutParams = {
    val (upperLeftShapeCoord, upperLeftPixelOffset) = shapeCoordAndOffset(upperLeftPixelCoord)
    val (lowerRightShapeCoord, lowerRightPixelOffset) = shapeCoordAndOffset(lowerRightPixelCoord)
    val shapeSpan = (lowerRightShapeCoord._1 - upperLeftShapeCoord._1 + 1, lowerRightShapeCoord._2 - upperLeftShapeCoord._2 + 1)
    val terrainSlice = getSliceFromGenerator(upperLeftShapeCoord, shapeSpan)
    TerrainSliceWithCutParams(terrainSlice, upperLeftPixelOffset, shapeSpan, lowerRightPixelOffset)
  }

  private def pixelsPerShape = tilePixels * Const.tilesPerShape

  private def absModulo(a: Int, b: Int) = if(a < 0) b + a % b else a % b

  private def fluentDiv(a: Int, b: Int) = if(a < 0) a / b - 1 else a / b

  private def shapeCoordAndOffset(mapPixelCoord: (Int, Int)): ((Int, Int), (Int, Int)) = {
    val pixPerShape = pixelsPerShape
    val shapeCoord = (fluentDiv(mapPixelCoord._1, pixPerShape), fluentDiv(mapPixelCoord._2, pixPerShape))
    val shapePixelOffset = (absModulo(mapPixelCoord._1, pixPerShape), absModulo(mapPixelCoord._2, pixPerShape))
    (shapeCoord, shapePixelOffset)
  }

  private def getSliceFromGenerator(upperLeftShapeCoord: (Int, Int), shapeSpan: (Int, Int)): Map[(Int, Int), Shape] = {
    val terrainMatrix = Seq.tabulate(shapeSpan._1 + 1, shapeSpan._2 + 1)((x, y) => (x, y)).flatten
    val terrainCoords = upperLeftShapeCoord match {
      case (dx, dy) => terrainMatrix.map { case (x, y) => (x + dx, y + dy) }
    }
    terrainCoords.map { case (dx, dy) =>
      ((dx - upperLeftShapeCoord._1, dy - upperLeftShapeCoord._2), Shape(ShapeGenWrapper.get(dx, dy)))
    }.toMap
  }
}

