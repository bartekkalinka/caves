package game

object Screen {
  private def pixelsToTilesOffset(screenOffs: (Int, Int), tilePixels: Int): (Int, Int) =
    (screenOffs._1 / tilePixels, screenOffs._2 / tilePixels)

  private def playerToLeftCorner(player: Player): (Int, Int) =
    (player.onMap._1 - player.onScreen._1, player.onMap._2 - player.onScreen._2)

  private def calculateCoordParams(player: Player, tilePixels: Int) = {
    val leftCornerCoord = playerToLeftCorner(player)
    val pixPerShape = Terrain.pixelsPerShape(tilePixels)
    val (_, leftPixelOffset) = Terrain.shapeCoordAndOffset(leftCornerCoord, tilePixels)
    val shapeSpan = ((leftPixelOffset._1 + Const.screenWidth) / pixPerShape, (leftPixelOffset._2 + Const.screenHeight) / pixPerShape)
    val rightPixelOffset = ((leftPixelOffset._1 + Const.screenWidth) % pixPerShape, (leftPixelOffset._2 + Const.screenHeight) % pixPerShape)
    (leftPixelOffset, shapeSpan, rightPixelOffset)
  }

  def calculate(player: Player, tilePixels: Int): Shape = {
    val (leftPixelOffset, shapeSpan, rightPixelOffset) = calculateCoordParams(player, tilePixels)
    val upperLeftCornerCoord = playerToLeftCorner(player)
    val lowerRightCornerCoord = (upperLeftCornerCoord._1 + Const.screenWidth, upperLeftCornerCoord._2 + Const.screenHeight)
    val terrain: Map[(Int, Int), Shape] = Terrain.getSliceFromGenerator(upperLeftCornerCoord, lowerRightCornerCoord, tilePixels)
    val cutParams = CutParams(pixelsToTilesOffset(leftPixelOffset, tilePixels),
      shapeSpan, pixelsToTilesOffset(rightPixelOffset, tilePixels))
    ShapeCutter.cut(terrain, cutParams)
  }
}