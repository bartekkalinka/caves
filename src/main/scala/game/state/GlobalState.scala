package game.state

import java.util.function.LongUnaryOperator

import game.Const
import java.util.concurrent.atomic.AtomicLong

case class SinglePlayerPublicState(position: (Int, Int))

object GlobalState {
  val generatedTerrain = new shapegen.Terrain(Const.shapeGenNeededLevel)

  private val playerCounter = new AtomicLong(0L)

  def getPlayerId(): Long = {
    def longUnaryOperator = new LongUnaryOperator {
      override def applyAsLong(operand: Long): Long = operand + 1L
    }
    playerCounter.updateAndGet(longUnaryOperator)
  }

  private val playersPublicData = new scala.collection.concurrent.TrieMap[Long, SinglePlayerPublicState]

  def publishState(playerId: Long, coord: (Int, Int)): Unit = {
    val publicState = SinglePlayerPublicState(coord)
    playersPublicData.put(playerId, publicState)
  }

  def getPositionsWithinRectangle(upperLeft: (Int, Int), lowerRight: (Int, Int)): Seq[(Long, SinglePlayerPublicState)] =
    playersPublicData.toSeq.filter { case (id, SinglePlayerPublicState(coord)) =>
      coord._1 >= upperLeft._1 && coord._2 >= upperLeft._2 && coord._1 < lowerRight._1 && coord._2 < lowerRight._2
    }
}