package game.calculations

import game.Const
import game.state.Shape

case class Tunnels(horizontal: Option[Int], vertical: Option[Int])

object Terrain {
  def init: Terrain = Terrain(new shapegen.Terrain(Const.shapeGenNeededLevel), Tunnels(None, None))
}

case class Terrain(generatedTerrain: shapegen.Terrain, tunnels: Tunnels) {
  def toggleTunnel(horizontal: Boolean, playerCoord: (Int, Int), tilePixels: Int): Terrain = {
    val playerTileCoord = ScreenCommon(tilePixels).tileCoordAndOffset(playerCoord)
    def toggleOneTunnel(tunnel: Option[Int], coordFun: ((Int, Int)) => Int): Option[Int] =
      tunnel match {
        case None => Some(coordFun(playerTileCoord.coord))
        case _ => None
      }
    Terrain(generatedTerrain = this.generatedTerrain, tunnels =
      if(horizontal) //TODO refactor
        Tunnels(toggleOneTunnel(tunnels.horizontal, _._2), tunnels.vertical)
      else
        Tunnels(tunnels.horizontal, toggleOneTunnel(tunnels.vertical, _._1))
    )
  }

  def isTileSet(mapPixelCoord: (Int, Int), tilePixels: Int): Boolean = {
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

  def cut(upperLeftCornerCoord: (Int, Int), lowerRightCornerCoord: (Int, Int), tilePixels: Int): Shape = {
    val upperLeftTileCoord = ScreenCommon(tilePixels).tileCoordAndOffset(upperLeftCornerCoord).coord
    val lowerRightTileCoord = ScreenCommon(tilePixels).tileCoordAndOffset(lowerRightCornerCoord).coord
    Shape((upperLeftTileCoord._1 to lowerRightTileCoord._1).map(tileX =>
      (upperLeftTileCoord._2 to lowerRightTileCoord._2).map(tileY =>
        isTileSet((tileX * tilePixels, tileY * tilePixels), tilePixels)).toArray).toArray
    )
  }
}

