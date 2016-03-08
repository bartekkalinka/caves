package game.calculations

import shapegen.ShapeGenWrapper

case class Terrain(tilePixels: Int) {
  def isTileSet(mapPixelCoord: (Int, Int)): Boolean = {
    val CoordAndOffset(shapeCoord, shapePixelOffset) = ScreenCommon(tilePixels).shapeCoordAndOffset(mapPixelCoord)
    val shape = ShapeGenWrapper.get(shapeCoord._1, shapeCoord._2)
    val CoordAndOffset(shapeTilesCoord, _) = ScreenCommon(tilePixels).tileCoordAndOffset(shapePixelOffset)
    shape(shapeTilesCoord._1)(shapeTilesCoord._2)
  }

//  def getSliceWithCutParams(upperLeftPixelCoord: (Int, Int), lowerRightPixelCoord: (Int, Int)): TerrainSliceWithCutParams = {
//    val CoordAndOffset(upperLeftShapeCoord, upperLeftPixelOffset) = ScreenCommon(tilePixels).shapeCoordAndOffset(upperLeftPixelCoord)
//    val CoordAndOffset(lowerRightShapeCoord, lowerRightPixelOffset) = ScreenCommon(tilePixels).shapeCoordAndOffset(lowerRightPixelCoord)
//    val shapeSpan = (lowerRightShapeCoord._1 - upperLeftShapeCoord._1 + 1, lowerRightShapeCoord._2 - upperLeftShapeCoord._2 + 1)
//    val terrainSlice = getSliceFromGenerator(upperLeftShapeCoord, shapeSpan)
//    TerrainSliceWithCutParams(terrainSlice, upperLeftPixelOffset, shapeSpan, lowerRightPixelOffset)
//  }
//
//  private def getSliceFromGenerator(upperLeftShapeCoord: (Int, Int), shapeSpan: (Int, Int)): Map[(Int, Int), Shape] = {
//    val terrainMatrix = Seq.tabulate(shapeSpan._1 + 1, shapeSpan._2 + 1)((x, y) => (x, y)).flatten
//    val terrainCoords = upperLeftShapeCoord match {
//      case (dx, dy) => terrainMatrix.map { case (x, y) => (x + dx, y + dy) }
//    }
//    terrainCoords.map { case (dx, dy) =>
//      ((dx - upperLeftShapeCoord._1, dy - upperLeftShapeCoord._2), Shape(ShapeGenWrapper.get(dx, dy)))
//    }.toMap
//  }
}

