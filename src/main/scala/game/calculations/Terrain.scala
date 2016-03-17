package game.calculations

import game.Const
import game.state.Shape

case class Tunnels(horizontal: Option[Int], vertical: Option[Int])

object Terrain {
  val generatedTerrain = new shapegen.Terrain(Const.shapeGenNeededLevel)

  def toggleTunnel(horizontal: Boolean, playerCoord: (Int, Int), tilePixels: Int, tunnels: Tunnels): Tunnels = {
    val playerTileCoord = ScreenCommon(tilePixels).tileCoordAndOffset(playerCoord)
    if(horizontal) //TODO refactor
      tunnels match {
        case Tunnels(None, v) => Tunnels(Some(playerTileCoord.coord._2), v)
        case Tunnels(_, v) => Tunnels(None, v)
      }
    else
      tunnels match {
        case Tunnels(h, None) => Tunnels(h, Some(playerTileCoord.coord._1))
        case Tunnels(h, _) => Tunnels(h, None)
      }
  }

  def isTileSet(mapPixelCoord: (Int, Int), tilePixels: Int, tunnels: Tunnels): Boolean = {
    def isTileEmptiedByTunnel: Boolean = {
      val CoordAndOffset(tileCoord, _) = ScreenCommon(tilePixels).tileCoordAndOffset(mapPixelCoord)
      //TODO refactor
      tunnels.horizontal.exists(tunnelYCoord =>
        Math.abs(tileCoord._2 - tunnelYCoord) <= Const.tunnelWidth / 2
      ) ||
      tunnels.vertical.exists(tunnelXCoord =>
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

  def cut(upperLeftCornerCoord: (Int, Int), lowerRightCornerCoord: (Int, Int), tilePixels: Int, tunnels: Tunnels): Shape = {
    val upperLeftTileCoord = ScreenCommon(tilePixels).tileCoordAndOffset(upperLeftCornerCoord).coord
    val lowerRightTileCoord = ScreenCommon(tilePixels).tileCoordAndOffset(lowerRightCornerCoord).coord
    Shape((upperLeftTileCoord._1 to lowerRightTileCoord._1).map(tileX =>
      (upperLeftTileCoord._2 to lowerRightTileCoord._2).map(tileY =>
        isTileSet((tileX * tilePixels, tileY * tilePixels), tilePixels, tunnels)).toArray).toArray
    )
  }
}

