package game.calculations

import game.Const
import game.state.Shape

object Terrain {
  val generatedTerrain = new shapegen.Terrain(Const.shapeGenNeededLevel)
  val artificialHoles = new scala.collection.mutable.HashMap[(Int, Int), Unit]

  def testHoles = Stream.from(0).take(1000).map((_, 0)).foreach(artificialHoles.put(_, ()))

  def isTileSet(mapPixelCoord: (Int, Int), tilePixels: Int): Boolean = {
    def isTileSetInArtificial: Boolean = {
      val CoordAndOffset(tileCoord, _) = ScreenCommon(tilePixels).tileCoordAndOffset(mapPixelCoord)
      artificialHoles.get(tileCoord).isEmpty
    }

    def isTileSetInGenerated: Boolean = {
      val CoordAndOffset(shapeCoord, shapePixelOffset) = ScreenCommon(tilePixels).shapeCoordAndOffset(mapPixelCoord)
      val shape = generatedTerrain.get(shapeCoord._1, shapeCoord._2).noise
      val CoordAndOffset(shapeTilesCoord, _) = ScreenCommon(tilePixels).tileCoordAndOffset(shapePixelOffset)
      shape(shapeTilesCoord._1)(shapeTilesCoord._2) >= Const.shapeGenThreshold
    }
    isTileSetInGenerated && isTileSetInArtificial
  }

  def cut(upperLeftCornerCoord: (Int, Int), lowerRightCornerCoord: (Int, Int), tilePixels: Int): Shape = {
    val upperLeftTileCoord = ScreenCommon(tilePixels).tileCoordAndOffset(upperLeftCornerCoord).coord
    val lowerRightTileCoord = ScreenCommon(tilePixels).tileCoordAndOffset(lowerRightCornerCoord).coord
    Shape((upperLeftTileCoord._1 to lowerRightTileCoord._1).map(tileX =>
      (upperLeftTileCoord._2 to lowerRightTileCoord._2).map(tileY =>
        isTileSet((tileX * tilePixels, tileY * tilePixels), tilePixels)).toArray).toArray
    )
  }
}

