package game.singleplayer

import game.Const
import game.calculations.{ScreenCommon, Terrain}
import game.multiplayer.GlobalState

case class SinglePlayerState(playerId: Long, playerFigure: PlayerFigure, score: Int, tilePixels: Int, terrain: Terrain)
{
  def applyMod(mod: StateMod): SinglePlayerState = mod match {
    case playerMod: PlayerMod => this.copy(playerFigure = playerFigure.applyPlayerMod(playerMod))
    case Zoom(in) =>
      val newTilePixels =
        if (in) math.floor(tilePixels * Const.zoomFactor).toInt
        else math.floor(tilePixels / Const.zoomFactor).toInt
      val newPlayerOnScreen = ScreenCommon(newTilePixels).tileEven(playerFigure.onScreen)
      this.copy(tilePixels = newTilePixels, playerFigure = playerFigure.copy(onScreen = newPlayerOnScreen, tilePixels = newTilePixels))
    case ToggleTunnel(horizontal) =>
      this.copy(terrain = terrain.toggleTunnel(horizontal, playerFigure.onMap, tilePixels))
  }

  def applyModsList(mods: Seq[StateMod]): SinglePlayerState =
    mods.foldLeft(this)((st, mod) => st.applyMod(mod))

  def resetDebug: SinglePlayerState = copy(playerFigure = playerFigure.resetDebug)

  def iteration(input: Option[UserInput]): SinglePlayerState = {
    val modsSuppliers: List[SinglePlayerState => Seq[StateMod]] =
      List(
        Step.vectorMods(input),
        Step.collisionMods,
        Step.coordMods
      )
    modsSuppliers.foldLeft(resetDebug)((st, modsSupplier) => st.applyModsList(modsSupplier(st)))
  }
}

object SinglePlayerState {
  def init: SinglePlayerState = {
    val initTerrain = Terrain.init
    SinglePlayerState(
      playerId = GlobalState.getPlayerId(),
      playerFigure = PlayerFigure.init(Const.initTilePixels, initTerrain),
      score = 0,
      tilePixels = Const.initTilePixels,
      terrain = initTerrain
    )
  }

  def iteration(state: SinglePlayerState, input: Option[UserInput]): SinglePlayerState = {
    val newState = state.iteration(input)
    GlobalState.publishState(newState.playerId, newState.playerFigure.onMap)
    newState
  }
}