package game.state

import game.Const
import game.calculations.Screen

case class UserInput(dir: String)

sealed trait FaceDirection
object FaceDirection {
  object Straight extends FaceDirection
  object Right extends FaceDirection
  object Left extends FaceDirection
}

sealed trait StateMod
case class Zoom(in: Boolean) extends StateMod
case class ToggleTunnel(horizontal: Boolean) extends StateMod
sealed trait PlayerMod extends StateMod
case class SetStandingOnGround(value: Boolean) extends PlayerMod
sealed trait VectorMod extends PlayerMod
case class SetPlayerHorizontalVector(mod: Int, faceDirection: FaceDirection = FaceDirection.Straight) extends VectorMod
case class SetPlayerVerticalVector(mod: Int) extends VectorMod
case class SetPlayerVectorLimitedByCollision(mod: (Int, Int)) extends VectorMod
case class ModifyPlayerVerticalVectorBy(acc: Int) extends VectorMod
sealed trait CoordMod extends PlayerMod
case class SetPlayerMapCoord(onMap: (Int, Int)) extends CoordMod
case class SetPlayerScreenCoord(onScreen: (Int, Int)) extends CoordMod

object Step {
  private def stateDrivenVectorMods(state: SinglePlayerState): Seq[VectorMod] =
    List(ModifyPlayerVerticalVectorBy(Const.gravityAcceleration)).filter(v => state.playerFigure.vector._2 <= Const.maxFallSpeed)

  private def movePlayerOnScreenIfStaysInTheMiddle(player: PlayerFigure): (Int, Int) = {
    val newPos = player.movePlayerOnScreenMod.onScreen
    if(Screen.isInTheMiddleOfScreen(newPos)) newPos else player.onScreen
  }

  private def inputDrivenModOpt(player: PlayerFigure, input: Option[UserInput]): Option[StateMod] = input.flatMap {
    case UserInput("rightKeyDown") => Some(SetPlayerHorizontalVector(Const.moveStepInPixels, FaceDirection.Right))
    case UserInput("upKeyDown") => Some(SetPlayerVerticalVector(-2 * Const.moveStepInPixels)).filter(x => player.onGround)
    case UserInput("leftKeyDown") => Some(SetPlayerHorizontalVector(-Const.moveStepInPixels, FaceDirection.Left))
    //case UserInput("zoomin") => Some(Zoom(true))
    //case UserInput("zoomout") => Some(Zoom(false))
    case UserInput("rightKeyUp") => Some(SetPlayerHorizontalVector(0))
    case UserInput("leftKeyUp") => Some(SetPlayerHorizontalVector(0))
    case UserInput("horizontal") => Some(ToggleTunnel(true))
    case UserInput("vertical") => Some(ToggleTunnel(false))
    case _ => None
  }

  def vectorMods(input: Option[UserInput])(state: SinglePlayerState): Seq[StateMod] =
    inputDrivenModOpt(state.playerFigure, input).toList ++ stateDrivenVectorMods(state)

  def collisionMods(state: SinglePlayerState): Seq[PlayerMod] =
    checkCollision(state).toList :+ checkIfStandingOnGround(state)

  private def checkCollision(state: SinglePlayerState): Option[VectorMod] = {
    val collision = state.playerFigure.isAtCollisionVector(state.terrain)
    collision.map(SetPlayerVectorLimitedByCollision)
  }

  private def checkIfStandingOnGround(state: SinglePlayerState): SetStandingOnGround = {
    val fallALittleVector = (0, Const.moveStepInPixels)
    val collisionWithGround = state.playerFigure.copy(vector = fallALittleVector).isAtCollisionVector(state.terrain)
    SetStandingOnGround(collisionWithGround.contains((0, 0)))
  }

  def coordMods(state: SinglePlayerState): Seq[CoordMod] = List(
    state.playerFigure.movePlayerOnMapMod,
    SetPlayerScreenCoord(movePlayerOnScreenIfStaysInTheMiddle(state.playerFigure))
  )
}