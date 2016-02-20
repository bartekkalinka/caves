package game.state

import game.Const
import game.calculations.Screen

case class UserInput(dir: String)
case class StepData(state: State, input: Option[UserInput])

sealed trait FaceDirection
object FaceDirection {
  object Straight extends FaceDirection
  object Right extends FaceDirection
  object Left extends FaceDirection
}

sealed trait StateMod
case class Zoom(in: Boolean) extends StateMod
sealed trait PlayerMod extends StateMod
case class SetStandingOnGround(value: Boolean) extends PlayerMod
sealed trait VectorMod extends PlayerMod
case class SetPlayerHorizontalVector(mod: Int, faceDirection: FaceDirection = FaceDirection.Straight) extends VectorMod
case class SetPlayerVerticalVector(mod: Int) extends VectorMod
case class SetPlayerVectorLimitedByCollision(mod: (Int, Int)) extends VectorMod
sealed trait CoordMod extends PlayerMod
case class SetPlayerMapCoord(onMap: (Int, Int)) extends CoordMod
case class SetPlayerScreenCoord(onScreen: (Int, Int)) extends CoordMod

object Step {
  private def addStateDrivenVectorMods(state: State): Seq[VectorMod] =
    List(SetPlayerVerticalVector(state.player.vector._2 + Const.gravityAcceleration)).filter(_.mod <= Const.maxFallSpeed)

  private def movePlayerOnScreenIfStaysInTheMiddle(player: Player): (Int, Int) = {
    val newPos = player.movePlayerOnScreenMod.onScreen
    if(Screen.isInTheMiddleOfScreen(newPos)) newPos else player.onScreen
  }

  private def inputDrivenModOpt(player: Player, input: Option[UserInput]): Option[VectorMod] = input.flatMap {
    case UserInput("rightKeyDown") => Some(SetPlayerHorizontalVector(Const.moveStepInPixels, FaceDirection.Right))
    case UserInput("upKeyDown") => Some(SetPlayerVerticalVector(-Const.moveStepInPixels)).filter(x => player.onGround)
    case UserInput("leftKeyDown") => Some(SetPlayerHorizontalVector(-Const.moveStepInPixels, FaceDirection.Left))
    case UserInput("rightKeyUp") => Some(SetPlayerHorizontalVector(0))
    case UserInput("leftKeyUp") => Some(SetPlayerHorizontalVector(0))
    case _ => None
  }

  def vectorMods(data: StepData): Seq[VectorMod] =
    inputDrivenModOpt(data.state.player, data.input).toList ++ addStateDrivenVectorMods(data.state)

  def collisionMods(state: State): Seq[PlayerMod] =
    checkCollision(state).toList :+ checkIfStandingOnGround(state)

  private def checkCollision(state: State): Option[VectorMod] = {
    val collision = state.player.isAtCollisionVector(state.tilePixels)
    collision.map(SetPlayerVectorLimitedByCollision)
  }

  private def checkIfStandingOnGround(state: State): SetStandingOnGround = {
    val playerVectorsVerticalCompound = (0, Const.moveStepInPixels)
    val collisionWithGround = state.player.copy(vector = playerVectorsVerticalCompound).isAtCollisionVector(state.tilePixels)
    SetStandingOnGround(collisionWithGround.contains((0, 0)))
  }

  def coordMods(state: State): Seq[CoordMod] = List(
    state.player.movePlayerOnMapMod,
    SetPlayerScreenCoord(movePlayerOnScreenIfStaysInTheMiddle(state.player))
  )
}