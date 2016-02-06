package game.calculations

case class CoordAndOffset(coord: (Int, Int), offset: (Int, Int))

object ScreenCommon {
  def fluentDiv(a: Int, b: Int) = if(a < 0) (a + 1) / b - 1 else a / b

  def absModulo(a: Int, b: Int) = if(a < 0) (b + a % b) % b else a % b

  def tileCoordAndOffset(screenOffs: (Int, Int), tilePixels: Int): CoordAndOffset =
    unitCoordAndOffset(screenOffs, tilePixels)

  def unitCoordAndOffset(pixelCoord: (Int, Int), pixelsPerUnit: Int): CoordAndOffset = {
    val unitCoord = (fluentDiv(pixelCoord._1, pixelsPerUnit), fluentDiv(pixelCoord._2, pixelsPerUnit))
    val unitPixelOffset = (absModulo(pixelCoord._1, pixelsPerUnit), absModulo(pixelCoord._2, pixelsPerUnit))
    CoordAndOffset(unitCoord, unitPixelOffset)
  }
}

