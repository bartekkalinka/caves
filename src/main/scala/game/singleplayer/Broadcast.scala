package game.singleplayer

import game.calculations.Screen
import game.multiplayer.GlobalState

case class Shape(tiles: Array[Array[Int]])
case class ScreenData(shape: Shape, offset: (Int, Int), otherPlayers: Seq[(Int, Int)])
case class PackedShape(tiles: Array[String])
case class PlayerOnScreen(x: Int, y: Int)

case class Broadcast(
  player: PlayerOnScreen,
  faceDirection: FaceDirection,
  tilePixels: Int,
  shape: PackedShape,
  offset: (Int, Int),
  otherPlayers: Seq[(Int, Int)],
  debugInfo: String
)

object Broadcast
{
  private def packShape(shape: Shape): PackedShape = PackedShape(shape.tiles.map(_.map(_.toString).reduce(_ + _)))

  private def debugInfo(state: SinglePlayerState, screenData: ScreenData): String = {
    def booleanToString(value: Boolean, name: String): String = s"$name:${value.toString.substring(0, 1)}"
    val playersCoords =
      GlobalState.getPositionsWithinRectangle((-200000, -200000), (200000, 200000)).map { case (id, crd) =>
        s"id:$id crd:$crd"
      }.mkString(" ")
    val collisionVectorStr = "col:" + state.playerFigure.debugInfo.collisionLimitedVector.map(_.toString()).getOrElse("")
    val onGroundStr = booleanToString(state.playerFigure.onGround, "gnd")
    val otherPlayers = screenData.otherPlayers.map { case (x, y) => s"($x, $y)" }.mkString(" ")
    s"$otherPlayers $playersCoords $onGroundStr $collisionVectorStr"
  }

  def fromState(state: SinglePlayerState): Broadcast = {
    val screenData = Screen.calculate(state.playerId, state.playerFigure, state.tilePixels, state.terrain)
    val ScreenData(shape, offset, otherPlayers) = screenData
    val playerOnScreen = PlayerOnScreen.tupled(state.playerFigure.onScreen)
    Broadcast(
      playerOnScreen, state.playerFigure.faceDirection, state.tilePixels, packShape(shape), offset, otherPlayers, debugInfo(state, screenData)
    )
  }
}
