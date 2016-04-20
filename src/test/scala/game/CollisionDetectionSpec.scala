package game.calculations

import org.scalatest._

class CollisionDetectionSpec extends FlatSpec with Matchers {
  it should "calculate line pixels correctly" in {
    CollisionDetection.bresenhamLineAlgorithm((0, 0), (3, 4)) should be (Seq((0,0), (1,1), (1,2), (2,3), (3,4)))
    CollisionDetection.bresenhamLineAlgorithm((0, 0), (-1, 4)) should be (Seq((0,0), (0,1), (0,2), (-1,3), (-1,4)))
    CollisionDetection.bresenhamLineAlgorithm((0, 0), (-11, -4)) should be (
      Seq((0,0), (-1,0), (-2,-1), (-3,-1), (-4,-1), (-5,-2), (-6,-2), (-7,-3), (-8,-3), (-9,-3), (-10,-4), (-11,-4))
    )
    CollisionDetection.bresenhamLineAlgorithm((4, -3), (7, 1)) should be (Seq((4,-3), (5,-2), (5,-1), (6,0), (7,1)))
  }
}

