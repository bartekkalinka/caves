package game.calculations

import game.Const
import game.state.Shape

case class Terrain(tilePixels: Int) {
  def isTileSet(mapPixelCoord: (Int, Int)): Boolean = {
    val CoordAndOffset(shapeCoord, shapePixelOffset) = ScreenCommon(tilePixels).shapeCoordAndOffset(mapPixelCoord)
    val shape = Terrain.terrain.get(shapeCoord._1, shapeCoord._2).noise
    val CoordAndOffset(shapeTilesCoord, _) = ScreenCommon(tilePixels).tileCoordAndOffset(shapePixelOffset)
    shape(shapeTilesCoord._1)(shapeTilesCoord._2) >= Const.shapeGenThreshold
  }

  def cut(upperLeftCornerCoord: (Int, Int), lowerRightCornerCoord: (Int, Int)): Shape = {
    val upperLeftTileCoord = ScreenCommon(tilePixels).tileCoordAndOffset(upperLeftCornerCoord).coord
    val lowerRightTileCoord = ScreenCommon(tilePixels).tileCoordAndOffset(lowerRightCornerCoord).coord
    Shape((upperLeftTileCoord._1 to lowerRightTileCoord._1).map(tileX =>
      (upperLeftTileCoord._2 to lowerRightTileCoord._2).map(tileY =>
        isTileSet(tileX * tilePixels, tileY * tilePixels)).toArray).toArray
    )
  }
}

object Terrain {
  val terrain = new shapegen.Terrain(Const.shapeGenNeededLevel)
}

