package game

object Const {
  private val baseTilesPerShape = 6
  private val targetNoiseDetail = 3
  private val baseMultiplier = Math.pow(2, Const.targetNoiseDetail).toInt
  val tilesPerShape = baseTilesPerShape * baseMultiplier
  val initTilePixels = 96 / baseMultiplier
  val moveStepInPixels = 8
  val maxFallSpeed = 4
  val gravityAcceleration = 1
  val zoomFactor = Math.sqrt(1.5)
  val screenWidth = 768
  val screenHeight = 500
  val shapeGenNeededLevel = 6
  val shapeGenThreshold = 500
  val tunnelWidth = 10
}
