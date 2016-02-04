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
case class SetPlayerVector(mod: (Int, Int), faceDirection: FaceDirection = FaceDirection.Straight) extends PlayerMod
case class SetPlayerMapCoord(onMap: (Int, Int)) extends PlayerMod
case class SetPlayerScreenCoord(onScreen: (Int, Int)) extends PlayerMod

object Step {
  private def stateDrivenMod(state: State): List[StateMod] = {
    val possibleMod = state.player.movePlayerOnMapMod
    if(!state.player.applyPlayerMod(possibleMod).isAtCollision(state.tilePixels)) {
      List(possibleMod, SetPlayerScreenCoord(movePlayerOnScreenIfStaysInTheMiddle(state.player)))
    }
    else {
      List[StateMod]()
    }
  }

  private def movePlayerOnScreenIfStaysInTheMiddle(player: Player): (Int, Int) = {
    val newPos = player.movePlayerOnScreenMod.onScreen
    if(Screen.isInTheMiddleOfScreen(newPos)) newPos else player.onScreen
  }

  private def inputDrivenModOpt(moveStepInPixels: Int, input: Option[UserInput]): Option[StateMod] = input.collect  {
    case UserInput("rightKeyDown") => SetPlayerVector((moveStepInPixels, 0), FaceDirection.Right)
    case UserInput("upKeyDown") => SetPlayerVector((0, -moveStepInPixels))
    case UserInput("leftKeyDown") => SetPlayerVector((-moveStepInPixels, 0), FaceDirection.Left)
    case UserInput("downKeyDown") => SetPlayerVector((0, moveStepInPixels))
    case UserInput("zoomin") => Zoom(true)
    case UserInput("zoomout") => Zoom(false)
    case ui: UserInput if ui.dir.endsWith("Up") => SetPlayerVector((0, 0))
  }

  def step(data: StepData): Seq[StateMod] = {
    stateDrivenMod(data.state) ++ inputDrivenModOpt(Const.moveStepInPixels, data.input).toList
  }
}