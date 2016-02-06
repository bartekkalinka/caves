package game.calculations

object ScreenCommon {
  def fluentDiv(a: Int, b: Int) = if(a < 0) (a + 1) / b - 1 else a / b

  def absModulo(a: Int, b: Int) = if(a < 0) (b + a % b) % b else a % b

  def pixelsToTilesOffset(screenOffs: (Int, Int), tilePixels: Int): (Int, Int) =
    (fluentDiv(screenOffs._1, tilePixels), fluentDiv(screenOffs._2, tilePixels))
}

