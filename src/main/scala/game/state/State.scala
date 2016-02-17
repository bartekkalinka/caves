package game.state

import game.Const
import game.calculations.Terrain

case class State(player: Player, score: Int, tilePixels: Int)
{
  def applyMod(mod: StateMod): State = mod match {
    case playerMod: PlayerMod => this.copy(player = player.applyPlayerMod(playerMod))
    case Zoom(in) =>
      val newTilePixels =
        if (in) math.floor(tilePixels * Const.zoomFactor).toInt
        else math.floor(tilePixels / Const.zoomFactor).toInt
      val newPlayerOnScreen = State.tileEven(newTilePixels, player.onScreen)
      this.copy(tilePixels = newTilePixels, player = player.copy(onScreen = newPlayerOnScreen))
  }

  def applyModsList(mods: Seq[StateMod]): State =
    mods.foldLeft(this)((st, mod) => st.applyMod(mod))
}

object State {
  def tileEven(tilePixels: Int, coord: (Int, Int)) = (coord._1 - coord._1 % tilePixels, coord._2 - coord._2 % tilePixels)

  private def initPlayerOnScreen(tilePixels: Int): (Int, Int) =
    tileEven(tilePixels, (Const.screenWidth / 2, Const.screenHeight / 2))

  private def initPlayerOnMap(tilePixels: Int): (Int, Int) =
    Stream.from(0).find(x => !Terrain(tilePixels).isTileSet((x, 0))).map((_, 0)).getOrElse((0, 0))

  def init: State = State(
    player = Player(
      onMap = initPlayerOnMap(Const.initTilePixels),
      vector = (0, 0),
      onScreen = initPlayerOnScreen(Const.initTilePixels),
      faceDirection = FaceDirection.Straight
    ),
    score = 0,
    tilePixels = Const.initTilePixels
  )

  def iteration(state: State, input: Option[UserInput]): State = {
    val stepData = StepData(state, input)
    val vectorMods = Step.vectorMods(stepData)
    val stateAfterVectorMods = state.applyModsList(vectorMods)
    val stateAfterCheckCollision = Step.checkCollision(stateAfterVectorMods).map(state.applyMod).getOrElse(stateAfterVectorMods)
    stateAfterVectorMods.applyModsList(Step.coordMods(stateAfterCheckCollision))
  }
}

