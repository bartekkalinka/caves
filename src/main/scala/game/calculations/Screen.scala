package game.calculations

import game.Const
import game.state.{Shape, Player}

object Screen {
  private def playerToLeftCorner(player: Player): (Int, Int) =
    (player.onMap._1 - player.onScreen._1, player.onMap._2 - player.onScreen._2)

  def isInTheMiddleOfScreen(coord: (Int, Int)): Boolean =
    math.abs(coord._1 - Const.screenWidth / 2) < Const.screenWidth / 4 &&
      math.abs(coord._2 - Const.screenHeight / 2) < Const.screenHeight / 4

  def fluentDiv(a: Int, b: Int) = if(a < 0) (a + 1) / b - 1 else a / b

  def absModulo(a: Int, b: Int) = if(a < 0) (b + a % b) % b else a % b

  def pixelsToTilesOffset(screenOffs: (Int, Int), tilePixels: Int): (Int, Int) =
    (fluentDiv(screenOffs._1, tilePixels), fluentDiv(screenOffs._2, tilePixels))

  def calculate(player: Player, tilePixels: Int): Shape = {
    val upperLeftCornerCoord = playerToLeftCorner(player)
    val lowerRightCornerCoord = (upperLeftCornerCoord._1 + Const.screenWidth, upperLeftCornerCoord._2 + Const.screenHeight)
    val cutParams = Terrain(tilePixels).getSliceWithCutParams(upperLeftCornerCoord, lowerRightCornerCoord)
    ShapeCutter(tilePixels).cut(cutParams)
  }
}