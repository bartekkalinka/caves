package game.calculations

import game.state.Shape

case class ShapeCutter(tilePixels: Int) {
    def cut(upperLeftCornerCoord: (Int, Int), lowerRightCornerCoord: (Int, Int)): Shape = {
      val upperLeftTileCoord = ScreenCommon(tilePixels).tileCoordAndOffset(upperLeftCornerCoord).coord
      val lowerRightTileCoord = ScreenCommon(tilePixels).tileCoordAndOffset(lowerRightCornerCoord).coord
      val terrain = Terrain(tilePixels)
      Shape((upperLeftTileCoord._1 to lowerRightTileCoord._1).map(tileX =>
        (upperLeftTileCoord._2 to lowerRightTileCoord._2).map(tileY =>
         terrain.isTileSet(tileX * tilePixels, tileY * tilePixels)).toArray).toArray
      )
    }
}


