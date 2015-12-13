package game

import pl.bka.shapegenweb.{Terrain, Config, Noise}

object ShapeGenWrapper {
  val neededLevel = 6

  val terrain = new Terrain(Config(0, neededLevel))

  private def getNoise(x: Int, y: Int, more: Boolean = false): Noise = {
    val current = if(!more) terrain.get(x, y) else terrain.moreDetail(x, y)
    if(current.level >= neededLevel) current else getNoise(x, y, true)
  }

  private def noiseToBoolArr(noise: Array[Array[Int]]): Array[Array[Boolean]] = noise.map(_.map(_ >= 500))

  private def boolArrToStrArr(boolArr: Array[Array[Boolean]]): Array[String] = boolArr.map(_.map(if(_) "1" else "0").reduce(_ + _))

  def get(x: Int, y: Int): Array[String] = boolArrToStrArr(noiseToBoolArr(getNoise(x, y).noise))
}

