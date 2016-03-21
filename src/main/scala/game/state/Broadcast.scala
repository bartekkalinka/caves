package game.state

import game.calculations.Screen

case class Shape(tiles: Array[Array[Int]])
case class ShapeWithOffset(shape: Shape, offset: (Int, Int))
case class PackedShape(tiles: Array[String])
case class PlayerOnScreen(x: Int, y: Int)

case class Broadcast(
  player: PlayerOnScreen,
  faceDirection: FaceDirection,
  tilePixels: Int,
  shape: PackedShape,
  offset: (Int, Int),
  debugInfo: String
)

object Broadcast
{
  private def packShape(shape: Shape): PackedShape = PackedShape(shape.tiles.map(_.map(_.toString).reduce(_ + _)))

  private def debugInfo(state: State): String = {
    def booleanToString(value: Boolean, name: String): String = s"$name:${value.toString.substring(0, 1)}"
    val playerOnMapStr = s"crd:${state.player.onMap}"
    val collisionVectorStr = "col:" + state.player.debugInfo.collisionLimitedVector.map(_.toString()).getOrElse("")
    val onGroundStr = booleanToString(state.player.onGround, "gnd")
    s"$playerOnMapStr $onGroundStr $collisionVectorStr"
  }

  def fromState(state: State): Broadcast = {
    val ShapeWithOffset(shape, offset) = Screen.calculate(state.player, state.tilePixels, state.terrain)
    val playerOnScreen = PlayerOnScreen.tupled(state.player.onScreen)
    Broadcast(
      playerOnScreen, state.player.faceDirection, state.tilePixels, packShape(shape), offset, debugInfo(state)
    )
  }
}
