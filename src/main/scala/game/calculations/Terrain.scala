package game.calculations

import game.Const
import game.state.Shape

case class Tunnel(coord: Option[Int], coordFun: ((Int, Int)) => Int)
case class Tunnels(byDir: Map[Boolean, Tunnel])

object Terrain {
  def init: Terrain = Terrain(new shapegen.Terrain(Const.shapeGenNeededLevel),
    Tunnels(Map(true -> Tunnel(None, _._2), false -> Tunnel(None, _._1))))
}

case class Terrain(generatedTerrain: shapegen.Terrain, tunnels: Tunnels) {
  def toggleTunnel(horizontal: Boolean, playerCoord: (Int, Int), tilePixels: Int): Terrain = {
    val playerTileCoord = ScreenCommon(tilePixels).tileCoordAndOffset(playerCoord)
    def toggleOneTunnel(tunnel: Tunnel): Tunnel =
      tunnel.copy(coord = tunnel.coord match {
        case None => Some(tunnel.coordFun(playerTileCoord.coord))
        case _ => None
      })
    Terrain(generatedTerrain = this.generatedTerrain, tunnels =
        tunnels.copy(byDir = tunnels.byDir +
          (horizontal -> toggleOneTunnel(tunnels.byDir(horizontal)))
    ))
  }

  def isTileSet(mapPixelCoord: (Int, Int), tilePixels: Int): Boolean = {
    def isTileEmptiedByTunnel: Boolean = {
      val CoordAndOffset(tileCoord, _) = ScreenCommon(tilePixels).tileCoordAndOffset(mapPixelCoord)
      Seq(true, false).map { dir =>
        val tunnel = tunnels.byDir(dir)
          tunnel.coord.exists(tunnelCoord =>
          Math.abs(tunnel.coordFun(tileCoord) - tunnelCoord) <= Const.tunnelWidth / 2
          )
      }.reduce(_ || _)
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

