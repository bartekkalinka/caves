
package shapegen

import scala.collection.mutable
import scala.util.Random

case class Noise(noise: Array[Array[Int]], detail: Int, level: Int) {
  override def toString = "Noise " + noise.foldLeft("")((s, a) => s + "[" + a.foldLeft("")((s, i) => s + i + " ") + "], ") + " detail " + detail

  def double: Noise =
    Noise(noise.flatMap(a => Array(a.flatMap(b => Array(b, b)), a.flatMap(b => Array(b, b)))), Noise.levDetail(level + 1), level + 1)

  def smooth(safeGet: (Int, Int) => Int): Noise =
    Noise(
      Range(0, noise.size).map({ x => Range(0, noise(0).size).map({ y =>
        Noise.smoothCoordMatrix.foldLeft[Int](0) {
          (sum, d) =>
            d match {
              case ((dx, dy), i) => sum + safeGet(x + dx, y + dy) / Noise.smoothWeightMatrix(i)
            }
        }}).toArray
      }).toArray
      , Noise.levDetail(level + 1), level + 1)

  def nextLevel(safeGet: (Array[Array[Int]], Int) => (Int, Int) => Int): Noise = if(level % 2 == 0) double else smooth(safeGet(noise, level + 1))
}


object Noise {
  val baseSize = 6

  val neighboursMatrix = List.tabulate(3, 3)((a, b) => (a - 1, b - 1)).flatten
  val smoothCoordMatrix = neighboursMatrix.zipWithIndex
  val smoothWeightMatrix = neighboursMatrix.map {case (a, b) => Math.pow(2, a.abs + b.abs + 2).toInt}

  def base: Noise = {
    Noise(Array.fill(baseSize, baseSize)(Random.nextInt(1000)), 0, 0)
  }

  def detailSize(detail: Int) = Noise.baseSize * Math.pow(2, detail).toInt

  def levDetail(lev: Int): Int = (lev + 1) / 2
}

class Terrain(neededLevel: Int) {
  val noiseCache = new mutable.HashMap[(Int, Int), Stream[Noise]]

  def reset = {
    noiseCache.clear()
  }

  private def noiseStream(x: Int, y: Int): Stream[Noise] = {
    def localGet = safeGet(x, y) _
    Stream.iterate(Noise.base)(_.nextLevel(localGet))
  }

  private def getCurrent(x: Int, y: Int): Stream[Noise] = {
    noiseCache.get(x, y) match {
      case Some(stream) => stream
      case None =>
        val stream = noiseStream(x, y)
        noiseCache.put((x, y), stream)
        stream
    }
  }

  def safeGet(shapeX: Int, shapeY: Int)(noise: Array[Array[Int]], level: Int)(tileX: Int, tileY: Int) = {
    val size = Noise.detailSize(Noise.levDetail(level))
    def toRealCoord(shapeCoord: Int, tileCoord: Int) =
      if (tileCoord < 0) (shapeCoord - 1, tileCoord + size)
      else
        if (tileCoord >= size) (shapeCoord + 1, tileCoord - size)
        else (shapeCoord, tileCoord)
    val (realShapeX, realTileX) = toRealCoord(shapeX, tileX)
    val (realShapeY, realTileY) = toRealCoord(shapeY, tileY)
    if (realShapeX != shapeX || realShapeY != shapeY) {
      val stream = getCurrent(realShapeX, realShapeY)
      val requiredLevel = requiredNeighbourLevel(level)
      stream(requiredLevel).noise(realTileX)(realTileY)
    } else {
      noise(tileX)(tileY)
    }
  }

  private def nextDetail(level: Int): Int = {
    level + (if(level % 2 == 0) 2 else 1)
  }

  private def requiredNeighbourLevel(level: Int): Int = {
    Math.max(if (level % 2 == 0) level - 1 else level - 2, 0)
  }

  def get(x: Int, y: Int): Noise = {
    val stream = getCurrent(x, y)
    stream(neededLevel)
  }

}