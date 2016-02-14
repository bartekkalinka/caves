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
case class SetPlayerHorizontalVector(mod: Int, faceDirection: FaceDirection = FaceDirection.Straight) extends PlayerMod
case class SetPlayerVerticalVector(mod: Int) extends PlayerMod
case class SetPlayerMapCoord(onMap: (Int, Int)) extends PlayerMod
case class SetPlayerScreenCoord(onScreen: (Int, Int)) extends PlayerMod

object Step {
  private def stateDrivenMod(state: State): List[StateMod] = {
    //TODO separation of vector mods from vector appliction
    //TODO collision should modify player vector
    val gravityVectorMod = List(SetPlayerVerticalVector(state.player.vector._2 + Const.gravityAcceleration)).filter(_.mod <= Const.maxFallSpeed)
    val collision = state.player.isAtCollisionVector(state.tilePixels)
    val playerWithCollisionLimitedVector = state.player.copy(vector = collision.getOrElse(state.player.vector))
    val vectorAndCollisionDetectionAppliedMods = List(
      playerWithCollisionLimitedVector.movePlayerOnMapMod,
      SetPlayerScreenCoord(movePlayerOnScreenIfStaysInTheMiddle(playerWithCollisionLimitedVector))
    )
    vectorAndCollisionDetectionAppliedMods ++ gravityVectorMod
  }

  private def movePlayerOnScreenIfStaysInTheMiddle(player: Player): (Int, Int) = {
    val newPos = player.movePlayerOnScreenMod.onScreen
    if(Screen.isInTheMiddleOfScreen(newPos)) newPos else player.onScreen
  }

  private def inputDrivenModOpt(moveStepInPixels: Int, input: Option[UserInput]): Option[StateMod] = input.collect  {
    case UserInput("rightKeyDown") => SetPlayerHorizontalVector(moveStepInPixels, FaceDirection.Right)
    case UserInput("upKeyDown") => SetPlayerVerticalVector(-moveStepInPixels)
    case UserInput("leftKeyDown") => SetPlayerHorizontalVector(-moveStepInPixels, FaceDirection.Left)
    case UserInput("zoomin") => Zoom(true)
    case UserInput("zoomout") => Zoom(false)
    case UserInput("rightKeyUp") => SetPlayerHorizontalVector(0)
    case UserInput("leftKeyUp") => SetPlayerHorizontalVector(0)
  }

  def step(data: StepData): Seq[StateMod] = {
    stateDrivenMod(data.state) ++ inputDrivenModOpt(Const.moveStepInPixels, data.input).toList
  }
}