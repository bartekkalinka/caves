package shapegen

import org.scalatest.{Matchers, FlatSpec}

class NoiseSpec extends FlatSpec with Matchers {
  "Noise.double" should "double coordinates" in {
    val noise = Noise(Array(Array(211, 424), Array(523, 989)), 0, 0)
    val dbl = noise.double
    dbl.noise.toList.map(_.toList) should be(List(
      List(211, 211, 424, 424),
      List(211, 211, 424, 424),
      List(523, 523, 989, 989),
      List(523, 523, 989, 989)
    ))
  }

  "Noise" should "keep correct detail to level relation" in {
    val noise = Noise(Array(Array(211, 424), Array(523, 989)), 0, 0)
    val noise2 = noise.double
    def dummyGet = (a: Int, b: Int) => 1
    noise2.detail should be (1)
    noise2.level should be (1)
    val noise3 = noise2.smooth(dummyGet)
    noise3.detail should be (1)
    noise3.level should be (2)
    val noise4 = noise3.double
    noise4.detail should be (2)
    noise4.level should be (3)
  }
}

class TerrainSpec extends FlatSpec with Matchers {

  "Terrain.get" should "cache results" in {
    val terrain = new Terrain(Config(0, 6))
    terrain.reset
    val noise1 = terrain.get(49, 51)
    val noise2 = terrain.get(49, 51)
    noise1 should be (noise2)
  }

  "Terrain" should "size noise based on its detail" in {
    val terrain = new Terrain(Config(0, 6))
    terrain.reset
    val noise1 = terrain.get(49, 51)
    val noise2 = terrain.moreDetail(49, 51)
    noise1.noise.length should be (noise2.noise.length / 2)
  }

  "Terrain.get" should "get detail 0 when used 1st time" in {
    val terrain = new Terrain(Config(0, 6))
    terrain.reset
    val noise = terrain.get(49, 51)
    noise.noise.length should be (Noise.baseSize)
    noise.detail should be (0)
  }

  "Terrain.get" should "get max detail version of given sector" in {
    val terrain = new Terrain(Config(0, 6))
    terrain.reset
    val noise1 = terrain.get(49, 51)
    val noise2 = terrain.moreDetail(49, 51)
    val noise3 = terrain.get(49, 51)
    noise3 should be (noise2)
    noise3.detail should be (1)
  }

  "Terrain.moreDetail" should "raise level of neighbouring sectors" in {
    val terrain = new Terrain(Config(0, 6))
    terrain.reset
    terrain.get(49, 51)
    terrain.moreDetail(49, 51)
    val noise1 = terrain.get(49, 50)
    noise1.level should be (1)
    noise1.detail should be (1)
    terrain.reset
    terrain.get(49, 51)
    terrain.moreDetail(49, 51)
    terrain.moreDetail(49, 51)
    val noise2 = terrain.get(49, 50)
    noise2.level should be (3)
    noise2.detail should be (2)
  }

  "Terrain.safeGet" should "read neihbours tiles when getting out of sector's boundaries" in {
    val terrain = new Terrain(Config(0, 6))
    terrain.reset
    terrain.get(49, 51)
    val noise = terrain.moreDetail(49, 51)
    val neighbourAbove = terrain.get(49, 50)
    val neighbourLeft = terrain.get(48, 51)
    val neighbourRight = terrain.get(50, 51)
    val neighbourBelow = terrain.get(49, 52)
    val localGet = terrain.safeGet(49, 51)(noise.noise, 2)
    val size = Noise.detailSize(Noise.levDetail(1))
    localGet(0, -1) should be (neighbourAbove.noise(0)(size - 1))
    localGet(-1, 0) should be (neighbourLeft.noise(size - 1)(0))
    localGet(size, 0) should be (neighbourRight.noise(0)(0))
    localGet(0, size) should be (neighbourBelow.noise(0)(0))
  }
}