package game.calculations

import game.Const
import game.state.Shape

object Terrain {
  val generatedTerrain = new shapegen.Terrain(Const.shapeGenNeededLevel)
  var horizontalTunnel: Option[Int] = None
  var verticalTunnel: Option[Int] = None

  def toggleTunnel(horizontal: Boolean, playerCoord: (Int, Int), tilePixels: Int) = {
    val playerTileCoord = ScreenCommon(tilePixels).tileCoordAndOffset(playerCoord)
    if(horizontal) //TODO refactor
      horizontalTunnel = horizontalTunnel match {
        case None => Some(playerTileCoord.coord._2)
        case _ => None
      }
    else
      verticalTunnel = verticalTunnel match {
        case None => Some(playerTileCoord.coord._1)
        case _ => None
      }
  }

  def isTileSet(mapPixelCoord: (Int, Int), tilePixels: Int): Boolean = {
    def isTileEmptiedByTunnel: Boolean = {
      val CoordAndOffset(tileCoord, _) = ScreenCommon(tilePixels).tileCoordAndOffset(mapPixelCoord)
      //TODO refactor
      horizontalTunnel.exists(tunnelYCoord =>
        Math.abs(tileCoord._2 - tunnelYCoord) <= Const.tunnelWidth / 2
      ) ||
      verticalTunnel.exists(tunnelXCoord =>
        Math.abs(tileCoord._1 - tunnelXCoord) <= Const.tunnelWidth / 2
      )
    }

    def isTileSetInGenerated: Boolean = {
      val CoordAndOffset(shapeCoord, shapePixelOffset) = ScreenCommon(tilePixels).shapeCoordAndOffset(mapPixelCoord)
      val shape = generatedTerrain.get(shapeCoord._1, shapeCoord._2).noise
      val CoordAndOffset(shapeTilesCoord, _) = ScreenCommon(tilePixels).tileCoordAndOffset(shapePixelOffset)
      shape(shapeTilesCoord._1)(shapeTilesCoord._2) >= Const.shapeGenThreshold
    }
    isTileSetInGenerated && !isTileEmptiedByTunnel
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

