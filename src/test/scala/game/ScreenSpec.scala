package game

import org.scalatest._

class ScreenSpec extends FlatSpec with Matchers {
  it should "cut displayed area" in {
    val terrain: Map[(Int, Int), Shape] = Map(
      (0, 0) -> Shape(Array(Array(true, false), Array(false, true))),
      (0, 1) -> Shape(Array(Array(false, false), Array(false, true))),
      (1, 0) -> Shape(Array(Array(true, true), Array(true, false))),
      (1, 1) -> Shape(Array(Array(true, false), Array(false, false)))
    )
    val tileOffset = (1, 1)
    ShapeCutter.cut(terrain, CutParams(tileOffset, (1, 1), tileOffset)).tiles.toSeq.map(_.toSeq) should be (
      Seq(Seq(true, false), Seq(true, true))
    )
  }

  it should "rehash correctly" in {
    val tab: Seq[Array[Int]] = List(
      Array(1, 2, 3),
      Array(4, 5, 6),
      Array(7, 8, 9)
    )
    ShapeCutter.rehash(tab) should be (
      Array(
        Seq(1, 4, 7),
        Seq(2, 5, 8),
        Seq(3, 6, 9)
      )
    )
  }
}

