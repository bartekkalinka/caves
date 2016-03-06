package shapegen

object ShapeGenWrapper {
  val neededLevel = 6

  val terrain = new Terrain(neededLevel)

  private def noiseToBoolArr(noise: Array[Array[Int]]): Array[Array[Boolean]] = noise.map(_.map(_ >= 500))

  def get(x: Int, y: Int): Array[Array[Boolean]] = noiseToBoolArr(terrain.get(x, y).noise)

  def reset = terrain.reset
}

