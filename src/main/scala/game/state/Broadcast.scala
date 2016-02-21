package game.state

import game.calculations.Screen

case class Shape(tiles: Array[Array[Boolean]])
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
  private def packShape(shape: Shape): PackedShape = PackedShape(shape.tiles.map(_.map(if(_) "1" else "0").reduce(_ + _)))

  private def debugInfo(state: State): String = {
    val collisionVectorStr = state.player.debugInfo.collisionLimitedVector.map(_.toString()).getOrElse("")
    s"${state.player.onGround} $collisionVectorStr"
  }

  def fromState(state: State): Broadcast = {
    val ShapeWithOffset(shape, offset) = Screen.calculate(state.player, state.tilePixels)
    val playerOnScreen = PlayerOnScreen.tupled(state.player.onScreen)
    Broadcast(
      playerOnScreen, state.player.faceDirection, state.tilePixels, packShape(shape), offset, debugInfo(state)
    )
  }
}
