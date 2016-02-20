package game.state

import game.Const
import game.calculations.{ScreenCommon, Terrain}

case class State(player: Player, score: Int, tilePixels: Int)
{
  def applyMod(mod: StateMod): State = mod match {
    case playerMod: PlayerMod => this.copy(player = player.applyPlayerMod(playerMod))
    case Zoom(in) =>
      val newTilePixels =
        if (in) math.floor(tilePixels * Const.zoomFactor).toInt
        else math.floor(tilePixels / Const.zoomFactor).toInt
      val newPlayerOnScreen = ScreenCommon(newTilePixels).tileEven(player.onScreen)
      this.copy(tilePixels = newTilePixels, player = player.copy(onScreen = newPlayerOnScreen))
  }

  def applyModsList(mods: Seq[StateMod]): State =
    mods.foldLeft(this)((st, mod) => st.applyMod(mod))
}

object State {
  def init: State = State(
    player = Player.initPlayer(Const.initTilePixels),
    score = 0,
    tilePixels = Const.initTilePixels
  )

  def iteration(state: State, input: Option[UserInput]): State = {
    val stepData = StepData(state, input)
    val vectorMods = Step.vectorMods(stepData)
    val stateAfterVectorMods = state.applyModsList(vectorMods)
    val stateAfterCollisionMods = stateAfterVectorMods.applyModsList(Step.collisionMods(stateAfterVectorMods))
    stateAfterVectorMods.applyModsList(Step.coordMods(stateAfterCollisionMods))
  }
}

