package game.calculations

import game.Const

case class Terrain(tilePixels: Int) {
  def isTileSet(mapPixelCoord: (Int, Int)): Boolean = {
    val CoordAndOffset(shapeCoord, shapePixelOffset) = ScreenCommon(tilePixels).shapeCoordAndOffset(mapPixelCoord)
    val shape = Terrain.terrain.get(shapeCoord._1, shapeCoord._2).noise
    val CoordAndOffset(shapeTilesCoord, _) = ScreenCommon(tilePixels).tileCoordAndOffset(shapePixelOffset)
    shape(shapeTilesCoord._1)(shapeTilesCoord._2) >= Const.shapeGenThreshold
  }
}

object Terrain {
  val terrain = new shapegen.Terrain(Const.shapeGenNeededLevel)
}

