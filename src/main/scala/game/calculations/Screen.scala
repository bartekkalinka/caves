package game.calculations

import game.Const
import game.state.{GlobalState, ScreenData, PlayerFigure}

object Screen {
  def isInTheMiddleOfScreen(coord: (Int, Int)): Boolean =
    math.abs(coord._1 - Const.screenWidth / 2) < Const.screenWidth / 4 &&
      math.abs(coord._2 - Const.screenHeight / 2) < Const.screenHeight / 4

  def calculate(playerId: Long, player: PlayerFigure, tilePixels: Int, terrain: Terrain): ScreenData = {
    def playerToLeftCorner(player: PlayerFigure): (Int, Int) =
      (player.onMap._1 - player.onScreen._1, player.onMap._2 - player.onScreen._2)
    val upperLeftCornerCoord = playerToLeftCorner(player)
    val lowerRightCornerCoord = (upperLeftCornerCoord._1 + Const.screenWidth + tilePixels, upperLeftCornerCoord._2 + Const.screenHeight + tilePixels)
    def otherPlayers = {
      val playersOnScreen = GlobalState.getPositionsWithinRectangle(upperLeftCornerCoord, lowerRightCornerCoord)
      playersOnScreen.filter(_._1 != playerId).map(_._2.position).map { case (x, y) => (x - upperLeftCornerCoord._1, y - upperLeftCornerCoord._2)}
    }
    val shape = terrain.cut(upperLeftCornerCoord, lowerRightCornerCoord, tilePixels)
    ScreenData(shape, ScreenCommon(tilePixels).tileCoordAndOffset(upperLeftCornerCoord).offset, otherPlayers)
  }
}