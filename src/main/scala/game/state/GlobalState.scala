package game.state

import java.util.function.LongUnaryOperator

import game.Const
import java.util.concurrent.atomic.AtomicLong

object GlobalState {
  val generatedTerrain = new shapegen.Terrain(Const.shapeGenNeededLevel)

  private val playerCounter = new AtomicLong(0L)

  def getPlayerId(): Long = {
    def longUnaryOperator = new LongUnaryOperator {
      override def applyAsLong(operand: Long): Long = operand + 1L
    }
    playerCounter.updateAndGet(longUnaryOperator)
  }
}