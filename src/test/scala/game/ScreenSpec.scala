package game

import game.calculations.Screen
import org.scalatest._

class ScreenSpec extends FlatSpec with Matchers {
  def test(a: Int, b: Int): (Int, Int) =
    (Screen.fluentDiv(a, b), Screen.absModulo(a, b))

  it should "calculate fluentDiv and absModulo correctly" in {
    test(4, 3) should be ((1, 1))
    test(3, 3) should be ((1, 0))
    test(2, 3) should be ((0, 2))
    test(1, 3) should be ((0, 1))
    test(0, 3) should be ((0, 0))
    test(-1, 3) should be ((-1, 2))
    test(-2, 3) should be ((-1, 1))
    test(-3, 3) should be ((-1, 0))
    test(-4, 3) should be ((-2, 2))
  }
}

