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

  private val playersCoords = new scala.collection.concurrent.TrieMap[Long, (Int, Int)]

  def putPosition(playerId: Long, coord: (Int, Int)) = playersCoords.put(playerId, coord)

  def getPositionsWithinRectangle(upperLeft: (Int, Int), lowerRight: (Int, Int)) =
    playersCoords.toSeq.filter { case (id, coord) =>
      coord._1 >= upperLeft._1 && coord._2 >= upperLeft._2 && coord._1 < lowerRight._1 && coord._2 < lowerRight._2
    }
}