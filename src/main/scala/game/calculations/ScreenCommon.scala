package game.calculations

import game.Const

case class CoordAndOffset(coord: (Int, Int), offset: (Int, Int))

object ScreenCommon {
  def applyVector(coord: (Int, Int), vector: (Int, Int)): (Int, Int) =
    (coord._1 + vector._1, coord._2 + vector._2)
}

case class ScreenCommon(tilePixels: Int) {
  def tileEven(coord: (Int, Int)) = (coord._1 - coord._1 % tilePixels, coord._2 - coord._2 % tilePixels)

  def fluentDiv(a: Int, b: Int) = if(a < 0) (a + 1) / b - 1 else a / b

  def absModulo(a: Int, b: Int) = if(a < 0) (b + a % b) % b else a % b

  private def pixelsPerShape = tilePixels * Const.tilesPerShape

  def shapeCoordAndOffset(mapPixelCoord: (Int, Int)): CoordAndOffset =
    unitCoordAndOffset(mapPixelCoord, pixelsPerShape)

  def tileCoordAndOffset(screenOffs: (Int, Int)): CoordAndOffset =
    unitCoordAndOffset(screenOffs, tilePixels)

  def unitCoordAndOffset(pixelCoord: (Int, Int), pixelsPerUnit: Int): CoordAndOffset = {
    val unitCoord = (fluentDiv(pixelCoord._1, pixelsPerUnit), fluentDiv(pixelCoord._2, pixelsPerUnit))
    val unitPixelOffset = (absModulo(pixelCoord._1, pixelsPerUnit), absModulo(pixelCoord._2, pixelsPerUnit))
    CoordAndOffset(unitCoord, unitPixelOffset)
  }
}

