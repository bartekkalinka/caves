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
    val terrain = new Terrain(6)
    terrain.reset
    val noise1 = terrain.get(49, 51)
    val noise2 = terrain.get(49, 51)
    noise1 should be (noise2)
  }

  "Terrain" should "size noise based on its detail" in {
    val terrainLowDetail = new Terrain(0)
    val terrainHigherDetail = new Terrain(2)
    terrainLowDetail.reset
    terrainHigherDetail.reset
    val noise1 = terrainLowDetail.get(49, 51)
    val noise2 = terrainHigherDetail.get(49, 51)
    noise1.noise.length should be (noise2.noise.length / 2)
  }

  "Terrain.get" should "get detail 0 when used 1st time" in {
    val terrain = new Terrain(0)
    terrain.reset
    val noise = terrain.get(49, 51)
    noise.noise.length should be (Noise.baseSize)
    noise.detail should be (0)
  }
}