package game.calculations

import shapegen.ShapeGenWrapper

case class Terrain(tilePixels: Int) {
  def isTileSet(mapPixelCoord: (Int, Int)): Boolean = {
    val CoordAndOffset(shapeCoord, shapePixelOffset) = ScreenCommon(tilePixels).shapeCoordAndOffset(mapPixelCoord)
    val shape = ShapeGenWrapper.get(shapeCoord._1, shapeCoord._2)
    val CoordAndOffset(shapeTilesCoord, _) = ScreenCommon(tilePixels).tileCoordAndOffset(shapePixelOffset)
    shape(shapeTilesCoord._1)(shapeTilesCoord._2)
  }
}

