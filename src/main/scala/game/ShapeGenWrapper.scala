package game

import pl.bka.shapegenweb.{Terrain, Config, Noise}

object ShapeGenWrapper {
  val neededLevel = 6

  val terrain = new Terrain(Config(0, neededLevel))

  private def getNoise(x: Int, y: Int, more: Boolean = false): Noise = {
    val current = if(!more) terrain.get(x, y) else terrain.moreDetail(x, y)
    if(current.level >= neededLevel) current else getNoise(x, y, true)
  }

  def get(x: Int, y: Int) = getNoise(x, y).noise.map { arr => arr.foldLeft("")((s, n) => s +(if(n >= 500) "1" else "0")) }
}

