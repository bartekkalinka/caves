package game

object Screen {
  private def playerToLeftCorner(player: Player): (Int, Int) =
    (player.onMap._1 - player.onScreen._1, player.onMap._2 - player.onScreen._2)

  def calculate(player: Player, tilePixels: Int): Shape = {
    val upperLeftCornerCoord = playerToLeftCorner(player)
    val lowerRightCornerCoord = (upperLeftCornerCoord._1 + Const.screenWidth, upperLeftCornerCoord._2 + Const.screenHeight)
    val cutParams = Terrain(tilePixels).getSliceWithCutParams(upperLeftCornerCoord, lowerRightCornerCoord)
    ShapeCutter(tilePixels).cut(cutParams)
  }
}