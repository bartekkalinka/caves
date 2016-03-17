package game.state

import game.Const
import game.calculations.{Tunnels, ScreenCommon, Terrain}

case class State(player: Player, score: Int, tilePixels: Int, tunnels: Tunnels)
{
  def applyMod(mod: StateMod): State = mod match {
    case playerMod: PlayerMod => this.copy(player = player.applyPlayerMod(playerMod))
    case Zoom(in) =>
      val newTilePixels =
        if (in) math.floor(tilePixels * Const.zoomFactor).toInt
        else math.floor(tilePixels / Const.zoomFactor).toInt
      val newPlayerOnScreen = ScreenCommon(newTilePixels).tileEven(player.onScreen)
      this.copy(tilePixels = newTilePixels, player = player.copy(onScreen = newPlayerOnScreen, tilePixels = newTilePixels))
    case ToggleTunnel(horizontal) =>
      this.copy(tunnels = Terrain.toggleTunnel(horizontal, player.onMap, tilePixels, tunnels))
  }

  def applyModsList(mods: Seq[StateMod]): State =
    mods.foldLeft(this)((st, mod) => st.applyMod(mod))

  def resetDebug: State = copy(player = player.resetDebug)
}

object State {
  def init: State = {
    Terrain.generatedTerrain.reset
    State(
      player = Player.initPlayer(Const.initTilePixels),
      score = 0,
      tilePixels = Const.initTilePixels,
      tunnels = Tunnels(None, None)
    )
  }

  def iteration(state: State, input: Option[UserInput]): State = {
    val modsSuppliers: List[State => Seq[StateMod]] =
      List(
        Step.vectorMods(input),
        Step.collisionMods,
        Step.coordMods
      )
    modsSuppliers.foldLeft(state.resetDebug)((st, modsSupplier) => st.applyModsList(modsSupplier(st)))
  }
}