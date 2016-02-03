package game.state

import game.calculations.Screen

case class Shape(tiles: Array[Array[Boolean]])
case class PackedShape(tiles: Array[String])
case class PlayerOnScreen(x: Int, y: Int)

case class Broadcast(player: PlayerOnScreen, faceDirection: FaceDirection, tilePixels: Int, shape: PackedShape)

object Broadcast
{
  private def packShape(shape: Shape): PackedShape = PackedShape(shape.tiles.map(_.map(if(_) "1" else "0").reduce(_ + _)))

  def fromState(state: State): Broadcast = {
    val shape = Screen.calculate(state.player, state.tilePixels)
    val playerOnScreen = PlayerOnScreen.tupled(state.player.onScreen)
    Broadcast(playerOnScreen, state.player.faceDirection, state.tilePixels, packShape(shape))
  }
}