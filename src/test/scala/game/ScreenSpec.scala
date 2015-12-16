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
    val result = Screen.cutDisplayed(terrain, tileOffset)
    Screen.cutDisplayed(terrain, tileOffset).tiles.toSeq.map(_.toSeq) should be (
      Seq(Seq(true, false), Seq(true, true))
    )
  }
}

