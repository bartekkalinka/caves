package game.calculations

import game.Const
import game.state.{ShapeWithOffset, Shape, Player}

object Screen {
  private def playerToLeftCorner(player: Player): (Int, Int) =
    (player.onMap._1 - player.onScreen._1, player.onMap._2 - player.onScreen._2)

  def isInTheMiddleOfScreen(coord: (Int, Int)): Boolean =
    math.abs(coord._1 - Const.screenWidth / 2) < Const.screenWidth / 4 &&
      math.abs(coord._2 - Const.screenHeight / 2) < Const.screenHeight / 4

  def calculate(player: Player, tilePixels: Int): ShapeWithOffset = {
    val upperLeftCornerCoord = playerToLeftCorner(player)
    val lowerRightCornerCoord = (upperLeftCornerCoord._1 + Const.screenWidth + tilePixels, upperLeftCornerCoord._2 + Const.screenHeight + tilePixels)
    val shape = ShapeCutter(tilePixels).cut(upperLeftCornerCoord, lowerRightCornerCoord)
    ShapeWithOffset(shape, ScreenCommon(tilePixels).tileCoordAndOffset(upperLeftCornerCoord).offset)
  }
}