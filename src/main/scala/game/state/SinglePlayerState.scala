package game.state

import game.Const
import game.calculations.{ScreenCommon, Terrain}

case class SinglePlayerState(player: Player, score: Int, tilePixels: Int, terrain: Terrain)
{
  def applyMod(mod: StateMod): SinglePlayerState = mod match {
    case playerMod: PlayerMod => this.copy(player = player.applyPlayerMod(playerMod))
    case Zoom(in) =>
      val newTilePixels =
        if (in) math.floor(tilePixels * Const.zoomFactor).toInt
        else math.floor(tilePixels / Const.zoomFactor).toInt
      val newPlayerOnScreen = ScreenCommon(newTilePixels).tileEven(player.onScreen)
      this.copy(tilePixels = newTilePixels, player = player.copy(onScreen = newPlayerOnScreen, tilePixels = newTilePixels))
    case ToggleTunnel(horizontal) =>
      this.copy(terrain = terrain.toggleTunnel(horizontal, player.onMap, tilePixels))
  }

  def applyModsList(mods: Seq[StateMod]): SinglePlayerState =
    mods.foldLeft(this)((st, mod) => st.applyMod(mod))

  def resetDebug: SinglePlayerState = copy(player = player.resetDebug)

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
      player = Player.init(Const.initTilePixels, initTerrain),
      score = 0,
      tilePixels = Const.initTilePixels,
      terrain = initTerrain
    )
  }

  def iteration(state: SinglePlayerState, input: Option[UserInput]): SinglePlayerState = state.iteration(input)
}