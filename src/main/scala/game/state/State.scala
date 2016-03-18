package game.state

import game.Const
import game.calculations.{Tunnels, ScreenCommon, Terrain}

case class State(player: Player, score: Int, tilePixels: Int, terrain: Terrain)
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
      this.copy(terrain = terrain.toggleTunnel(horizontal, player.onMap, tilePixels))
  }

  def applyModsList(mods: Seq[StateMod]): State =
    mods.foldLeft(this)((st, mod) => st.applyMod(mod))

  def resetDebug: State = copy(player = player.resetDebug)
}

object State {
  def init: State = {
    val initTerrain = Terrain.init
    State(
      player = Player.init(Const.initTilePixels, initTerrain),
      score = 0,
      tilePixels = Const.initTilePixels,
      terrain = initTerrain
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