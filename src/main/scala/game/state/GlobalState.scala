package game.state

import game.Const

object GlobalState {
  val generatedTerrain = new shapegen.Terrain(Const.shapeGenNeededLevel)
}