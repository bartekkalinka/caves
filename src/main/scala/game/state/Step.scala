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
sealed trait VectorMod extends PlayerMod
case class SetPlayerHorizontalVector(mod: Int, faceDirection: FaceDirection = FaceDirection.Straight) extends VectorMod
case class SetPlayerVerticalVector(mod: Int) extends VectorMod
case class SetPlayerVectorLimitedByCollision(mod: (Int, Int)) extends VectorMod
sealed trait CoordMod extends PlayerMod
case class SetPlayerMapCoord(onMap: (Int, Int)) extends CoordMod
case class SetPlayerScreenCoord(onScreen: (Int, Int)) extends CoordMod

object Step {
  private def addStateDrivenVectorMods(state: State, inputDrivenModOption: Option[VectorMod]): Seq[VectorMod] = {
    val gravityVectorMod = List(SetPlayerVerticalVector(state.player.vector._2 + Const.gravityAcceleration)).filter(_.mod <= Const.maxFallSpeed)
    val collision = state.player.isAtCollisionVector(state.tilePixels)
    val collisionModOption = collision.map(SetPlayerVectorLimitedByCollision)
    collisionModOption.map(List[VectorMod](_)).getOrElse(inputDrivenModOption.toList ++ gravityVectorMod)
  }

  private def movePlayerOnScreenIfStaysInTheMiddle(player: Player): (Int, Int) = {
    val newPos = player.movePlayerOnScreenMod.onScreen
    if(Screen.isInTheMiddleOfScreen(newPos)) newPos else player.onScreen
  }

  private def inputDrivenModOpt(moveStepInPixels: Int, input: Option[UserInput]): Option[VectorMod] = input.collect  {
    //TODO jump possible only when standing on something
    case UserInput("rightKeyDown") => SetPlayerHorizontalVector(moveStepInPixels, FaceDirection.Right)
    case UserInput("upKeyDown") => SetPlayerVerticalVector(-moveStepInPixels)
    case UserInput("leftKeyDown") => SetPlayerHorizontalVector(-moveStepInPixels, FaceDirection.Left)
    case UserInput("rightKeyUp") => SetPlayerHorizontalVector(0)
    case UserInput("leftKeyUp") => SetPlayerHorizontalVector(0)
  }

  def vectorMods(data: StepData): Seq[VectorMod] = {
    addStateDrivenVectorMods(data.state, inputDrivenModOpt(Const.moveStepInPixels, data.input))
  }

  def coordMods(state: State): Seq[CoordMod] = List(
    state.player.movePlayerOnMapMod,
    SetPlayerScreenCoord(movePlayerOnScreenIfStaysInTheMiddle(state.player))
  )
}